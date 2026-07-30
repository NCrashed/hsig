-- | Самый маленький трек, который имеет смысл: одна партия, одно окно.
-- Отсюда удобно начинать свой, см. README.
module Minimal (main) where

import Sound.Sig

kick :: Instrument
kick n =
  sine (constant (noteFreq n) * (1 + 6 * expdecay 0.02))
    * adsr 0.001 0.15 0 0.02 0.2

track :: [Stem]
track = [stem "kick" (play kick (slow 2 "a1*4") * gate 0.01 8)]

main :: IO ()
main = renderTrack defaultEnv "out/minimal.wav" track >>= putStrLn
