-- | Демо-трек: подряд всё, что умеет синтезатор на текущем этапе.
module Demo (main) where

import Control.Category ((>>>))
import Data.Function ((&))
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

main :: IO ()
main = do
  report <- writeWav env Bits16 path track
  putStrLn (path <> ": пик " <> show (clipPeak report))
  where
    path = "out/demo.wav"
