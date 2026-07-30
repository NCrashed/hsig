-- | Реактор: пульсирующие стержни, вращающиеся вокруг головы.
module Reactor
  ( reactor
  , rod
  ) where

import Data.Function ((&))
import Sound.Sig

-- | Стержни на заданных частотах. orbitHz это обороты в секунду, pulseHz
-- вспышки в секунду, spread это доля оборота между соседними стержнями.
--
-- Про spread стоит знать неочевидное. Развести стержни поровну по кругу
-- (spread = 1\/n) выглядит правильно, но на слух это почти неподвижный
-- образ: усиления трёх источников, разнесённых на 120 градусов, взаимно
-- гасятся, и суммарная картинка стоит на месте. Работает обратное:
-- spread = 0, то есть стержни летят вместе и вспыхивают по очереди. Тогда
-- каждая следующая вспышка приходит ровно на @orbitHz \/ (n * pulseHz)@
-- оборота дальше предыдущей, и луч ровным шагом обходит голову.
reactor :: Double -> Double -> Double -> [Double] -> Stereo
reactor orbitHz pulseHz spread freqs =
  mixStereo
    [ rod orbitHz pulseHz f (spread * fromIntegral i) (fromIntegral i / n)
    | (i, f) <- zip [0 :: Int ..] freqs
    ]
  where
    n = fromIntegral (max 1 (length freqs))

-- | Один стержень: turn это его место на орбите в долях оборота, flash это
-- сдвиг фазы вспышки в долях периода вспышек.
rod :: Double -> Double -> Double -> Double -> Double -> Stereo
rod orbitHz pulseHz freq turn flash = orbit angle voice
  where
    angle = phase (constant orbitHz) + constant (2 * pi * turn)

    -- Вспышка: приподнятый косинус в шестой степени, то есть узкий луч, а
    -- не ровное дыхание. Входит в определение дважды, но share тут нет
    -- намеренно: это сигнал длиной в трек, а пересчитать косинус со
    -- степенью дешевле, чем держать его целиком (та же оговорка, что у
    -- pan и orbit).
    pulse =
      mapSig (\x -> ((1 + cos x) / 2) ** 6) $
        phase (constant pulseHz) + constant (2 * pi * flash)

    -- Стержень должен читаться узкой полосой, поэтому резонанс высокий, а
    -- катофф открывается на вспышке. Оверсэмплинг вокруг фильтра обязателен:
    -- tanh в его обратной связи иначе заворачивает верх.
    voice =
      saw (constant freq)
        & oversample 4 (ladder (constant (freq * 2) + constant (freq * 8) * pulse) 0.9)
        & (* (pulse * constant 0.25))
