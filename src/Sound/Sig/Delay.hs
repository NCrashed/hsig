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
import Sound.Sig.Resample (besselI0, kaiserBeta)

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
-- Чтение дробное, ядро это оконный sinc на 16 отводов с окном Кайзера, как
-- в 'Sound.Sig.Resample'. Дешёвая кубическая интерполяция тут не годится:
-- её ослабление зависит от дробной части (на полусэмпле -3.25 дБ на
-- 16 кГц, на целых позициях ноль), а на движущемся источнике дробная часть
-- пробегает целые десятки раз в секунду - на слух это дрожание верха.
--
-- Замер ядра, ослабление на полусэмпле против целой задержки при 48 кГц:
-- до 12 кГц не измеряется, на 16 кГц -0.007 дБ, на 18 кГц -0.226,
-- на 20 кГц -1.47. То есть в слышимой полосе дрожания больше нет.
--
-- ЦЕНА: минимальная задержка это половина ядра, 8 сэмплов (167 мкс при
-- 48 кГц, 42 мкс при 192). Симметричному ядру нужны отсчёты по обе стороны
-- от точки чтения, а на меньшей задержке половина из них ещё не записана.
-- Меньшее время молча зажимается до минимума: оно задаётся сигналом и может
-- нырять под минимум посэмплово, падать тут нечем - та же конвенция, что у
-- зажима среза в фильтрах. Практический вывод: фленджер сквозь ноль этой
-- задержкой не собрать, самый короткий провал садится на 3 кГц. Хорусу,
-- доплеру, эху и межушной разнице минимум не мешает.
--
-- Длину сохраняет, обрезая по короткому из входа и управляющего сигнала:
-- это модуляционная задержка, а не эхо. Нужен хвост за концом входа -
-- добавьте padSec перед ней.
vdelay :: Double -> Sig -> Fx
vdelay maxSec ctrl input = Sig $ \env ->
  let rate = envRate env
      size = max (2 * fracTaps) (ceiling (max 0 maxSec * rate) + fracTaps)
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
      -- Отсчёт, отстоящий на m шагов назад от только что записанного.
      back pos m = UM.unsafeRead ring ((pos - m + 2 * size) `mod` size)
      loop !i !pos
        | i >= n = pure pos
        | otherwise = do
            UM.unsafeWrite ring pos (U.unsafeIndex xs i)
            let q = clampDelay size (U.unsafeIndex ctrl i * rate)
                (k, p) = q `divMod` fracPhases
                taps !t !acc
                  | t >= fracTaps = pure acc
                  | otherwise = do
                      v <- back pos (k + t - fracLead)
                      let h = U.unsafeIndex fracTable (p * fracTaps + t)
                      taps (t + 1) (acc + h * v)
            y <- taps 0 0
            UM.unsafeWrite out i y
            loop (i + 1) (if pos + 1 >= size then 0 else pos + 1)
  posEnd <- loop 0 pos0
  ringEnd <- U.unsafeFreeze ring
  o <- U.unsafeFreeze out
  pure (o, Line ringEnd posEnd)
  where
    n = U.length xs

-- | Отводов в ядре, фаз в таблице и сколько отводов лежит перед точкой
-- чтения. Тысяча фаз это шаг задержки в тысячную сэмпла, то есть 0.02 мкс
-- при 48 кГц - на три порядка мельче, чем различает слух.
fracTaps, fracPhases, fracLead :: Int
fracTaps = 16
fracPhases = 1024
fracLead = fracTaps `div` 2 - 1

-- | Задержка в долях сэмпла, зажатая в окно, которое ядро может прочитать.
-- NaN уходит в нижний край, как и у зажимов в фильтрах.
clampDelay :: Int -> Double -> Int
clampDelay size d
  | isNaN d = lo
  | otherwise = max lo (min hi (round (d * fromIntegral fracPhases)))
  where
    lo = (fracLead + 1) * fracPhases
    hi = (size - fracTaps + fracLead) * fracPhases

-- | Таблица ядер: на каждую фазу свой сдвинутый sinc под окном Кайзера.
--
-- Считается один раз на программу, 16 тысяч чисел. Сумма отводов
-- нормирована в единицу: постоянная составляющая не должна зависеть от
-- фазы, иначе движение задержки давало бы дрожание уровня.
fracTable :: U.Vector Double
fracTable = U.concat (map row [0 .. fracPhases - 1])
  where
    half = fromIntegral (fracTaps `div` 2)
    beta = kaiserBeta 90
    row p = U.map (/ U.sum ts) ts
      where
        f = fromIntegral p / fromIntegral fracPhases
        ts = U.generate fracTaps (\t -> tap (fromIntegral (t - fracLead) - f))
    tap x = sinc x * window x
    sinc x
      | abs x < 1e-12 = 1
      | otherwise = sin (pi * x) / (pi * x)
    window x
      | abs r >= 1 = 0
      | otherwise = besselI0 (beta * sqrt (1 - r * r)) / besselI0 beta
      where
        r = x / half

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
