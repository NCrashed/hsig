-- | Код главы 2: осцилляторы и алиасинг.
module Book.Ch02
  ( examples
  , naiveSaw
  , sweep
  , shapes
  , pulses
  ) where

import Book.Prelude
import Sound.Sig

naiveSaw :: Sig -> Sig
naiveSaw f = mapSig (\p -> p / pi - 1) (phase f)

sweep :: (Sig -> Sig) -> Sig
sweep osc = osc (200 * exp (line [(0, 0), (5, log 20)])) * 0.2 * gate 0.01 5

shapes :: Sig
shapes = mix [delay (fromIntegral i) (voice o) | (i, o) <- zip [0 :: Int ..] waves]
  where
    waves = [saw, square, tri, pulse 0.15]
    voice o = o 110 * 0.2 * gate 0.01 0.9

pulses :: Sig
pulses = pulse width 110 * 0.2 * gate 0.01 4
  where
    width = 0.5 + 0.45 * sine 0.5

examples :: [Example]
examples =
  [ example "02-saw-naive" (sweep naiveSaw)
  , example "02-saw-additive" (sweep saw)
  , example "02-shapes" shapes
  , example "02-pwm" pulses
  ]
