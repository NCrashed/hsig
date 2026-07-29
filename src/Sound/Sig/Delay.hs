-- | Задержки.
--
-- Линия задержки живёт в кольцевом буфере внутри runST, наружу торчит
-- только Fx (разд. 2 дизайна).
module Sound.Sig.Delay
  ( delay
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
