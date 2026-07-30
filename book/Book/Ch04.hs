-- | Код главы 4: фильтры.
module Book.Ch04
  ( examples
  , ladderSweep
  , resonant
  , svfModes
  , hat
  ) where

import Book.Prelude
import Sound.Sig

ladderSweep :: Sig
ladderSweep = ladder cut 0.7 (saw 110 * 0.5) * gate 0.01 6
  where
    cut = 120 * exp (line [(0, 0), (3, log 60), (6, 0)])

resonant :: Double -> Sig
resonant q = ladder cut (constant q) (saw 110 * 0.5) * gate 0.01 3
  where
    cut = 150 * exp (line [(0, 0), (3, log 40)])

svfModes :: Sig
svfModes = mix [delay (2 * fromIntegral i) (voice f) | (i, f) <- zip [0 :: Int ..] modes]
  where
    modes = [svf, svfBand, svfHigh, svfNotch]
    voice f = f 800 0.7 (noise 1 * 0.4) * gate 0.01 1.9

hat :: Sig
hat = highpass 7000 (noise 2) * adsr 0.001 0.05 0 0.01 0.08 * 0.5

examples :: [Example]
examples =
  [ example "04-ladder-sweep" ladderSweep
  , example "04-resonance-low" (resonant 0.2)
  , example "04-resonance-high" (resonant 0.95)
  , example "04-svf-modes" svfModes
  , example "04-hats" (mix [delay (0.25 * fromIntegral i) hat | i <- [0 :: Int .. 7]])
  ]
