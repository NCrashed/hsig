-- | Код главы 8: стерео и пространство.
module Book.Ch08
  ( examples
  , sides
  , wide
  , spinning
  , doubled
  ) where

import Book.Ch06 (kit)
import Book.Prelude
import Sound.Sig

sides :: Stereo
sides = mixStereo [pan (-0.8) left, pan 0.8 right]
  where
    left = play kit "hh*8" * gate 0.01 4
    right = play kit "~ sn ~ sn" * gate 0.01 4

wide :: Stereo
wide = Stereo (voice (-4)) (voice 4)
  where
    voice cents = saw (220 * constant (2 ** (cents / 1200))) * adsr 0.01 0.2 0.6 0.3 3 * 0.25

spinning :: Stereo
spinning = orbit (2 * pi * 0.5 * ramp) src
  where
    ramp = line [(0, 0), (8, 8)]
    src = play kit "bd*2" * gate 0.01 8 + saw 220 * adsr 0.01 0.3 0.4 0.5 8 * 0.2

doubled :: Stereo
doubled = Stereo src (vdelay 0.05 (constant 0.019) (src * 0.9))
  where
    src = share (play kit "hh*8" * gate 0.01 4)

examples :: [Example]
examples =
  [ exampleWide "08-pan" sides
  , exampleWide "08-wide" wide
  , exampleWide "08-orbit" spinning
  , exampleWide "08-haas" doubled
  ]
