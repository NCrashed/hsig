-- | Код главы 7: динамика и накачка.
module Book.Ch07
  ( examples
  , kickSig
  , bassSig
  , flat
  , pumping
  , squashed
  ) where

import Book.Ch06 (kit)
import Book.Prelude
import Sound.Sig

kickSig :: Sig
kickSig = share (play kit "bd*4" * gate 0.01 4)

bassSig :: Sig
bassSig = play bass (slow 2 "a1 a1 d2 a1") * gate 0.01 4
  where
    bass n =
      ladder (150 + 1800 * expdecay 0.09) 0.7 (saw (constant (noteFreq n)) * 0.5)
        * adsr 0.005 0.1 0.6 0.06 (noteDur n * 0.9)
        * 0.5

flat :: Sig
flat = kickSig + bassSig

pumping :: Sig
pumping = kickSig + sidechain kickSig 0.8 bassSig

squashed :: Sig
squashed = compress 0.05 8 0.003 0.05 bassSig * 1.6

examples :: [Example]
examples =
  [ example "07-flat" flat
  , example "07-pumping" pumping
  , example "07-bass-dry" bassSig
  , example "07-bass-compressed" squashed
  ]
