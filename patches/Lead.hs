-- | Приёмочный патч: цепочка из разд. 9 дизайна.
--
-- Порядок здесь не декоративный. Дисторшн стоит строго после фильтра:
-- гармоники нелинейности рождаются вокруг резонансного пика и едут вместе
-- с ним при свипе. Обратный порядок даёт жирный, но мёртвый звук.
module Lead
  ( lead
  , unison
  ) where

import Control.Category ((>>>))
import Data.Function ((&))
import Sound.Sig

-- | Расстроенный стек пил.
--
-- Голоса разведены по интервалу в cents в обе стороны, и у каждого свой
-- медленный дрейф: синус 0.05-0.15 Гц глубиной в цент, со своей частотой и
-- фазой. Без дрейфа стек звучит статично.
unison :: Int -> Double -> Sig -> Sig
unison n cents freq =
  sum (map voice [0 .. n - 1]) * constant (1 / fromIntegral n)
  where
    -- Частота входит в определение n раз, поэтому share (разд. 3).
    sfreq = share freq
    voice i = saw (sfreq * constant (ratio i) * drift i)

    ratio i = 2 ** (offsetCents i / 1200)
    offsetCents i
      | n <= 1 = 0
      | otherwise = negate cents + 2 * cents * fromIntegral i / fromIntegral (n - 1)

    drift i = 1 + constant centDepth * lfo i
    centDepth = 2 ** (1 / 1200) - 1
    -- Фазу задаём через phase: у sine её нет, а без разной фазы голоса
    -- стартовали бы синхронно.
    lfo i = mapSig sin (phase (constant (driftHz i)) + constant (driftPhase i))
    driftHz i = 0.05 + 0.1 * fromIntegral i / fromIntegral (max 1 (n - 1))
    driftPhase i = 2 * pi * 0.37 * fromIntegral i

-- | Лид из разд. 9.
lead :: Instrument
lead n = body * aEnv * constant (noteAmp n) + crushed
  where
    freq = constant (noteFreq n)
    -- Спад на квинту за 30 мс.
    pEnv = 1 + 0.5 * expdecay 0.03
    cut = 200 + 5200 * expdecay 0.09
    -- Отклонение от разд. 9: нота отпускается так, чтобы вместе с релизом
    -- уложиться ровно в свой слот. adsr добавляет релиз поверх dur, поэтому
    -- по букве разд. 9 хвост каждой ноты ложился бы на атаку следующей на
    -- всех переходах и границах тактов; паузами в паттерне это не лечится.
    -- На быстрых нотах релиз ужимается вместе со слотом.
    --
    -- Огибающая входит в определение дважды, поэтому share (разд. 3).
    rel = min 0.1 (0.5 * noteDur n)
    aEnv = share (adsr 0.002 0.4 0.6 rel (noteDur n - rel))

    -- Отклонение от разд. 9: после асимметричного шейпера стоит блокиратор
    -- постоянной составляющей. Без него DC от cubicnl усиливался бы
    -- гребёнкой в 1/(1-0.6) раз и съедал запас по громкости.
    body =
      share $
        unison 7 18 (freq * pEnv)
          & oversample 8 (ladder cut 0.85 >>> cubicnl 0.9 0.15)
          & highpass 20
          & comb 0.011 0.6

    -- Параллельный слой с редукцией подмешивается на 20 процентов.
    crushed = decimate 6 4 body * aEnv * constant (0.2 * noteAmp n)
