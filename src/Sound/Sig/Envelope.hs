-- | Огибающие.
--
-- Все они конечны (кроме expdecay), и это несущая идея: длительность ноты
-- задаётся умножением на огибающую, потому что (*) обрезает по короткому
-- (разд. 3 дизайна).
module Sound.Sig.Envelope
  ( line
  , expseg
  , adsr
  , expdecay
  ) where

import Data.Vector.Unboxed qualified as U
import Sound.Sig.Core

-- | Линейные брейкпоинты (время в секундах, значение). Время абсолютное от
-- начала, список должен идти по возрастанию. Сигнал кончается на последней
-- точке, до первой держится её значение.
line :: [(Double, Double)] -> Sig
line = segmented lerp
  where
    lerp v0 v1 x = v0 + (v1 - v0) * x

-- | Экспоненциальные сегменты. Значения не могут быть нулём или менять
-- знак: экспонента через ноль не проходит.
expseg :: [(Double, Double)] -> Sig
expseg pts
  | any (\(_, v) -> v == 0) pts = error "hsig: expseg не принимает нулевые значения"
  | not (sameSign (map snd pts)) = error "hsig: expseg не принимает смену знака"
  | otherwise = segmented geom pts
  where
    sameSign vs = all (> 0) vs || all (< 0) vs
    geom v0 v1 x = v0 * (v1 / v0) ** x

-- | Кусочно заданная огибающая: interp v0 v1 x, где x идёт от 0 до 1 внутри
-- сегмента.
segmented :: (Double -> Double -> Double -> Double) -> [(Double, Double)] -> Sig
segmented _ [] = Sig (const [])
segmented _ pts
  | not (nonDecreasing (map fst pts)) =
      error ("hsig: времена брейкпоинтов должны возрастать, задано " <> show (map fst pts))
  where
    nonDecreasing ts = and (zipWith (<=) ts (drop 1 ts))
segmented interp pts@((_, firstV) : _) = Sig $ \env ->
  let rate = envRate env
      n = round (fst (last pts) * rate) :: Int
      go !i segs
        | i >= n = []
        | otherwise = case segs of
            -- После последней точки держим её значение.
            [(_, v)] -> v : go (i + 1) segs
            (t0, v0) : more@((t1, v1) : _)
              | t >= t1 -> go i more
              | t < t0 -> firstV : go (i + 1) segs
              | otherwise -> interp v0 v1 ((t - t0) / (t1 - t0)) : go (i + 1) segs
            [] -> []
        where
          t = fromIntegral i / rate
   in runSig (fromSamples (go 0 pts)) env

-- | Классическая огибающая: атака a, спад d до уровня s, полка до dur,
-- дальше релиз r до нуля. Полная длина dur + r: нота отпускается в dur.
--
-- Если нота короче атаки со спадом, релиз стартует с того уровня, где её
-- застало отпускание.
adsr :: Double -> Double -> Double -> Double -> Double -> Sig
adsr a d s r dur = line (held <> [(dur, level), (dur + r, 0)])
  where
    full = [(0, 0), (a, 1), (a + d, s)]
    held = takeWhile ((< dur) . fst) full
    level = valueAt full dur

-- | Значение кусочно-линейной функции в точке t, вне диапазона держится
-- крайнее значение.
valueAt :: [(Double, Double)] -> Double -> Double
valueAt [] _ = 0
valueAt [(_, v)] _ = v
valueAt ((t0, v0) : more@((t1, v1) : _)) t
  | t <= t0 = v0
  | t >= t1 = valueAt more t
  | otherwise = v0 + (v1 - v0) * (t - t0) / (t1 - t0)

-- | Мгновенная атака и экспоненциальный спад с постоянной времени tau.
-- Бесконечная: конечная обрезала бы всё, на что её умножают.
expdecay :: Double -> Sig
expdecay tau
  | tau <= 0 = error ("hsig: постоянная времени должна быть положительной, задано " <> show tau)
  | otherwise = Sig $ \env ->
      let b = blockOf env
          k = 1 / (envRate env * tau)
          chunk j = U.generate b (\i -> exp (negate (fromIntegral (j * b + i) * k)))
       in map chunk [0 ..]
