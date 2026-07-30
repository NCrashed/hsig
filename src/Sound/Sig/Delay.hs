-- | Задержки.
--
-- Линия задержки живёт в кольцевом буфере внутри runST, наружу торчит
-- только Fx (разд. 2 дизайна).
module Sound.Sig.Delay
  ( delay
  , vdelay
  , comb
  , allpass
  ) where

import Control.Monad.ST (ST, runST)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Sound.Sig.Block (align2)
import Sound.Sig.Core

-- | Сдвиг во времени. Сигнал именно сдвигается, поэтому и длиннее ровно на
-- сдвиг, в отличие от гребёнки, которая длину сохраняет.
delay :: Double -> Fx
delay t (Sig g) = Sig $ \env ->
  let d = max 0 (round (t * envRate env))
   in if d == 0
        then g env
        else rechunk (blockOf env) (U.replicate d 0 : g env)

-- | Задержка с изменяемым временем: время задаётся сигналом в секундах.
--
-- Чтение дробное, интерполяция кубическая (Catmull-Rom). Округление до
-- целого сэмпла тут не годится: на плавно едущем времени оно даёт ступеньку
-- в 21 микросекунду при 48 кГц, то есть щелчки на каждом шаге. Линейной
-- интерполяции тоже мало, она заметно давит верх на дробных позициях.
--
-- Длину сохраняет, обрезая по короткому из входа и управляющего сигнала:
-- это модуляционная задержка, а не эхо. Нужен хвост за концом входа -
-- добавьте padSec перед ней.
--
-- Время зажимается в @[1\/rate, maxSec]@: кубической интерполяции нужен
-- сосед с обеих сторон, поэтому нулевой задержки не бывает.
vdelay :: Double -> Sig -> Fx
vdelay maxSec ctrl input = Sig $ \env ->
  let rate = envRate env
      size = max 8 (ceiling (max 0 maxSec * rate) + 4)
      go _ [] = []
      go line ((c, x) : rest) = out : go line' rest
        where
          (out, line') = runVdelay rate c x line
   in rechunk (blockOf env) (go (Line (U.replicate size 0) 0) (align2 (runSig ctrl env) (runSig input env)))

runVdelay :: Double -> U.Vector Double -> U.Vector Double -> Line -> (U.Vector Double, Line)
runVdelay rate ctrl xs (Line ring0 pos0) = runST $ do
  ring <- U.thaw ring0
  out <- UM.new n
  let size = UM.length ring
      -- Отсчёт, отстоящий на k шагов назад от только что записанного.
      back pos k = UM.unsafeRead ring ((pos - k + 2 * size) `mod` size)
      loop !i !pos
        | i >= n = pure pos
        | otherwise = do
            UM.unsafeWrite ring pos (U.unsafeIndex xs i)
            let d = clampDelay size (U.unsafeIndex ctrl i * rate)
                k = floor d :: Int
                f = d - fromIntegral k
            y0 <- back pos (k - 1)
            y1 <- back pos k
            y2 <- back pos (k + 1)
            y3 <- back pos (k + 2)
            UM.unsafeWrite out i (catmull f y0 y1 y2 y3)
            loop (i + 1) (if pos + 1 >= size then 0 else pos + 1)
  posEnd <- loop 0 pos0
  ringEnd <- U.unsafeFreeze ring
  o <- U.unsafeFreeze out
  pure (o, Line ringEnd posEnd)
  where
    n = U.length xs

-- | Задержка в сэмплах: не меньше сэмпла и не больше буфера. NaN уходит в
-- нижний край, как и у зажимов в фильтрах.
clampDelay :: Int -> Double -> Double
clampDelay size d = max 1 (min (fromIntegral (size - 3)) d)

-- | Catmull-Rom между y1 и y2, f это доля от y1 к y2.
catmull :: Double -> Double -> Double -> Double -> Double -> Double
catmull f y0 y1 y2 y3 =
  y1 + 0.5 * f * ((y2 - y0) + f * (2 * y0 - 5 * y1 + 4 * y2 - y3 + f * (3 * (y1 - y2) + y3 - y0)))

-- | Гребёнка с обратной связью: @y[n] = x[n] + fb*y[n-d]@.
comb :: Double -> Sig -> Fx
comb = lineFx step
  where
    step ring pos fb x = do
      old <- UM.unsafeRead ring pos
      let y = x + fb * old
      UM.unsafeWrite ring pos y
      pure y

-- | Фазовый фильтр Шрёдера: модуль отклика единичный, меняется только фаза.
--
-- @w[n] = x[n] + g*w[n-d]@, @y[n] = w[n-d] - g*w[n]@.
allpass :: Double -> Sig -> Fx
allpass = lineFx step
  where
    step ring pos g x = do
      old <- UM.unsafeRead ring pos
      let w = x + g * old
      UM.unsafeWrite ring pos w
      pure (old - g * w)

-- | Состояние линии: кольцевой буфер и позиция записи.
data Line = Line !(U.Vector Double) !Int

-- | Обвязка для эффектов с линией задержки. Длину сохраняет, обрезая по
-- более короткому из входа и управляющего сигнала.
lineFx
  :: (forall s. UM.MVector s Double -> Int -> Double -> Double -> ST s Double)
  -> Double
  -> Sig
  -> Fx
lineFx step t ctrl input = Sig $ \env ->
  let d = max 1 (round (t * envRate env))
      go _ [] = []
      go line ((c, x) : rest) = out : go line' rest
        where
          (out, line') = runLine step c x line
   in rechunk (blockOf env) (go (Line (U.replicate d 0) 0) (align2 (runSig ctrl env) (runSig input env)))

runLine
  :: (forall s. UM.MVector s Double -> Int -> Double -> Double -> ST s Double)
  -> U.Vector Double
  -> U.Vector Double
  -> Line
  -> (U.Vector Double, Line)
runLine step ctrl xs (Line ring0 pos0) = runST $ do
  ring <- U.thaw ring0
  out <- UM.new n
  let d = UM.length ring
      loop !i !pos
        | i >= n = pure pos
        | otherwise = do
            y <- step ring pos (U.unsafeIndex ctrl i) (U.unsafeIndex xs i)
            UM.unsafeWrite out i y
            loop (i + 1) (if pos + 1 >= d then 0 else pos + 1)
  posEnd <- loop 0 pos0
  ringEnd <- U.unsafeFreeze ring
  o <- U.unsafeFreeze out
  pure (o, Line ringEnd posEnd)
  where
    n = U.length xs
