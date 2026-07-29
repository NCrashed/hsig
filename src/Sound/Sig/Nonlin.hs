-- | Нелинейности.
--
-- Все они дают гармоники выше исходной полосы, поэтому в тракте их место
-- внутри oversample: без него верхние продукты заворачиваются (разд. 6.4).
module Sound.Sig.Nonlin
  ( shaper
  , cubicnl
  , clip
  , decimate
  ) where

import Sound.Sig.Block (stateful1)
import Sound.Sig.Core

-- | Мягкое насыщение @tanh (drive*x) \/ tanh drive@. Нормировка держит
-- полную шкалу на месте: единица остаётся единицей при любом drive.
shaper :: Sig -> Fx
shaper = zipChunks Truncate step
  where
    step drive x
      -- Предел при drive к нулю это тождество, а не деление нуля на ноль.
      | d < 1e-6 = x
      | otherwise = tanh (d * x) / tanh d
      where
        d = abs drive

-- | Асимметричный кубический шейпер: drive и смещение.
--
-- Смещение вносит чётные гармоники. Вычитается ровно та постоянная
-- составляющая, которую смещение даёт на тишине, поэтому тишина остаётся
-- тишиной и нота не заканчивается щелчком. Цена в том, что диапазон выхода
-- сдвигается: по одной полке он уходит за единицу на величину вычтенного.
--
-- Зависящий от сигнала DC этим не убирается: асимметричная кривая на
-- переменном входе даёт среднее около @-0.75*drive^2*bias*A^2@, где A это
-- амплитуда. После такого шейпера нужен блокиратор постоянной составляющей,
-- иначе гребёнка усилит её своим усилением на нуле герц.
cubicnl :: Sig -> Sig -> Fx
cubicnl drive bias x =
  zipChunks Truncate (-) (mapSig cubic driven) (mapSig cubic sbias)
  where
    -- Смещение входит в определение дважды, поэтому share: правило разд. 3.
    sbias = share bias
    driven = zipChunks Truncate (+) (zipChunks Truncate (*) drive x) sbias

-- | Кубическая полка: @1.5*(u - u^3\/3)@ внутри единицы, дальше насыщение.
-- Множитель нормирует полную шкалу.
cubic :: Double -> Double
cubic u
  | u <= -1 = -1
  | u >= 1 = 1
  | otherwise = 1.5 * (u - u * u * u / 3)

-- | Жёсткое ограничение по порогу.
clip :: Double -> Fx
clip t = mapSig (max (negate a) . min a)
  where
    a = abs t

-- | Редукция битов и частоты: квантование по 2^bits уровням и удержание
-- каждого сэмпла hold раз.
decimate :: Int -> Int -> Fx
decimate bits hold
  | bits < 1 = error ("hsig: битов должно быть не меньше 1, задано " <> show bits)
  | hold < 1 = error ("hsig: удержание должно быть не меньше 1, задано " <> show hold)
  | otherwise = stateful1 (0, 0 :: Int) step
  where
    scale = 2 ^ (bits - 1) :: Double
    step (held, counter) x
      | counter > 0 = (held, (held, counter - 1))
      | otherwise = let q = quantize x in (q, (q, hold - 1))
    quantize x = fromIntegral (round (x * scale) :: Int) / scale
