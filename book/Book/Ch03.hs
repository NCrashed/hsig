-- | Код главы 3: огибающие и ноты.
module Book.Ch03
  ( examples
  , pluck
  , pad
  , kick
  , riff
  , plucked
  , byHand
  ) where

import Book.Prelude
import Sound.Sig

pluck :: Double -> Sig
pluck f = saw (constant f) * adsr 0.002 0.12 0.2 0.15 0.6 * 0.3

pad :: Double -> Sig
pad f = saw (constant f) * adsr 0.4 0.3 0.7 0.8 2.5 * 0.15

kick :: Sig
kick = sine (55 * (1 + 6 * expdecay 0.02)) * adsr 0.001 0.15 0 0.02 0.2 * 0.9

riff :: Sig
riff = mix [delay (0.25 * fromIntegral i) (pluck f) | (i, f) <- zip [0 :: Int ..] phrase]
  where
    phrase = [220, 277.18, 330, 220, 165, 220, 277.18, 330]

plucked :: Instrument
plucked n =
  saw (constant (noteFreq n))
    * adsr 0.002 0.12 0.2 0.15 (noteDur n)
    * constant (noteAmp n * 0.3)

byHand :: Sig
byHand = mix [delay (0.3 * fromIntegral i) (plucked (note f d)) | (i, (f, d)) <- zip [0 :: Int ..] score]
  where
    note f d = (noteOf f) {noteDur = d}
    score = [(220, 0.28), (261.63, 0.28), (329.63, 0.55), (220, 0.9)]

examples :: [Example]
examples =
  [ example "03-riff" riff
  , example "03-pad" (mix [pad f | f <- [110, 164.81, 220]])
  , example "03-kick" (mix [delay (0.5 * fromIntegral i) kick | i <- [0 :: Int .. 3]])
  , example "03-plucked" byHand
  ]
