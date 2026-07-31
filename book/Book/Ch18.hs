-- | Код главы 18: лады, аккорды, арпеджио.
module Book.Ch18
  ( examples
  , runUp
  , transposed
  , chords
  , arpUp
  , progression
  ) where

import Book.Ch16 (rhodes, warmBass)
import Book.Prelude
import Sound.Sig

runUp :: Sig
runUp = play rhodes (slow 2 (scale "minor" "a3" "0 1 2 3 4 5 6 7")) * gate 0.01 8

transposed :: Sig
transposed = play rhodes (slow 2 (scale "minor" "a3" (every 2 (fmap (+ 2)) "0 2 4 2"))) * gate 0.01 8

chords :: Sig
chords = play rhodes (slow 2 "a3'min c4'maj e3'min7 f3'maj7") * 0.45 * gate 0.01 16

arpUp :: Sig
arpUp = play rhodes (arp "updown" (slow 2 "a3'min c4'maj e3'min7 f3'maj7")) * 0.7 * gate 0.01 16

progression :: Stereo
progression = bothChannels (\c -> shaper 1.2 (c * gate 0.05 16 * 0.45) * 0.9) mixed
  where
    harmony = slow 4 "a3'min f3'maj7 c4'maj e3'min7"
    keys = play rhodes (arp "up" (fast 2 harmony)) * 1.1
    pad = play rhodes harmony * 0.5
    bass = play warmBass (slow 4 (scale "minor" "a1" "0 ~ ~ ~ 5 ~ ~ ~ 2 ~ ~ ~ 4 ~ ~ ~")) * 1.2
    mixed = mixStereo [pan (-0.3) keys, pan 0.25 pad, mono bass]

examples :: [Example]
examples =
  [ example "18-degrees" runUp
  , example "18-transposed" transposed
  , example "18-chords" chords
  , example "18-arp" arpUp
  , exampleWide "18-progression" progression
  ]
