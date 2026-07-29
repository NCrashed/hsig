-- | Демо-трек: подряд всё, что умеет синтезатор на текущем этапе.
module Demo (main) where

import Control.Category ((>>>))
import Data.Function ((&))
import Lead (lead)
import Sound.Sig

env :: Env
env = defaultEnv

rate :: Double
rate = envRate env

-- | Нота: длительность задаёт огибающая, потому что (*) обрезает по
-- короткому.
voice :: (Sig -> Sig) -> Double -> Double -> Sig
voice wave freq dur = wave (constant freq) * adsr 0.01 0.15 0.6 0.08 dur * 0.4

-- | Щипок: мгновенная атака и экспоненциальный спад. Хвост уже неслышим,
-- когда трапеция закрывает ноту.
pluck :: Double -> Double -> Sig
pluck freq dur = saw (constant freq) * expdecay 0.15 * trapezoid dur * 0.5

-- | Прямоугольник с краями по 10 мс, чтобы не щёлкало на стыках.
trapezoid :: Double -> Sig
trapezoid dur = line [(0, 0), (edge, 1), (dur - edge, 1), (dur, 0)]
  where
    edge = 0.01

-- | Пила через лестничный фильтр: катофф едет сверху вниз, резонанс поёт.
filtered :: Double -> Sig
filtered dur = ladder cut 0.85 (saw 110 * 0.3) * trapezoid dur
  where
    cut = line [(0, 8000), (dur, 200)]

-- | Почти цепочка из разд. 9: фильтр с резонансом и шейпер внутри
-- oversample, следом гребёнка. Катофф тянется дольше ноты, чтобы срез
-- задержки в oversample не обрезал хвост огибающей.
gritty :: Double -> Sig
gritty dur =
  saw 110
    * 0.3
    & oversample 8 (ladder cut 0.8 >>> shaper 4)
    & comb 0.011 0.6
    & (* (trapezoid dur * 0.3))
  where
    cut = line [(0, 6000), (dur + 0.1, 300)]

-- | Свип 20 - 8000 Гц: на нём слышно, что гармоники не заворачиваются.
sweep :: Double -> Sig
sweep dur = saw (fromSamples freqs) * trapezoid dur * 0.4
  where
    n = round (dur * rate) :: Int
    freqs = [20 + (8000 - 20) * fromIntegral i / fromIntegral n | i <- [0 .. n - 1]]

-- | Склейка встык. Микшера и планировщика ещё нет, они на M6.
oneAfterAnother :: [Sig] -> Sig
oneAfterAnother = fromSamples . concatMap (samples env)

track :: Sig
track =
  oneAfterAnother
    [ voice sine 440 1
    , voice saw 110 1.5
    , voice square 110 1.5
    , voice tri 110 1.5
    , voice (const (noise 0)) 0 1
    , pluck 220 1.5
    , filtered 3
    , gritty 3
    , sweep 4
    ]

-- Ритм из паттернов ---------------------------------------------------------

-- | Такт это два цикла: при четырёх долях в цикле выходит 120 ударов в
-- минуту. Темп задаётся паттерном, отдельного поля под него нет.
bar :: Pattern a -> Pattern a
bar = slow 2

-- | Бас: пила через лестничный фильтр с быстрой огибающей катоффа.
bassInst :: Instrument
bassInst n =
  ladder cut 0.7 (saw (constant (noteFreq n)) * 0.5)
    * adsr 0.005 0.1 0.5 0.06 (noteDur n * 0.9)
    * constant (noteAmp n * 0.6)
  where
    cut = 200 + 2500 * expdecay 0.07

-- | Хэт: отфильтрованный шум с мгновенной атакой.
hatInst :: Instrument
hatInst n =
  highpass 6000 (noise 1)
    * adsr 0.001 0.04 0 0.01 (noteDur n * 0.4)
    * constant (noteAmp n * 0.25)

-- | Шестнадцать тактов по два цикла: 32 секунды при 120 ударах в минуту.
trackSec :: Double
trackSec = 32

-- | Трапеция по краям обязательна: рендер режет по времени жёстко, и без
-- неё последняя нота обрывается на середине релиза, то есть щелчком.
--
-- Лид это приёмочный патч разд. 9, бас и хэт держат ритм под ним.
sixteenBars :: [Stem]
sixteenBars =
  [ Stem "bass" (play bassInst bassPat * gate)
  , Stem "hat" (play hatInst hatPat * gate)
  , Stem "lead" (play lead leadPat * gate * 0.5)
  ]
  where
    gate = trapezoid trackSec
    bassPat = bar (listToPat (map noteOf [55, 55, 73.42, 55]))
    hatPat = bar (fast 8 (degradeBy 0.25 (pure (noteOf 1))))
    -- Пауза в третьей доле, чтобы фраза дышала.
    leadPat =
      bar . every 4 rev $
        fastcat [pure (noteOf 440), pure (noteOf 523.25), silence, pure (noteOf 659.25)]

main :: IO ()
main = do
  report <- writeWav env Bits16 tour track
  putStrLn (tour <> ": пик " <> show (clipPeak report))
  trackPath <- renderTrack env trackSec "out/track.wav" sixteenBars
  putStrLn (trackPath <> ": 16 тактов, лид по разд. 9")
  where
    tour = "out/demo.wav"
