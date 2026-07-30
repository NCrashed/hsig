-- | Код главы 17: IDM и глитч.
module Book.Ch17
  ( examples
  , bar
  , glitchKit
  , stutter
  , shuffled
  , grain
  , loop
  ) where

import Book.Ch06 (kit)
import Book.Prelude
import Sound.Sig

-- | 92 удара в минуту: не танцевальный темп, зато есть место для мелкой
-- нарезки.
bar :: Pattern a -> Pattern a
bar = slow (240 / 92)

glitchKit :: Instrument
glitchKit n = case noteLabel n of
  "bd" -> shaper 2.5 (sine (48 * (1 + 7 * expdecay 0.015)) * 0.9) * adsr 0.001 0.12 0 0.02 0.16 * 0.8
  "sn" -> shaper 2 ((onepole 7000 (highpass 900 (noise 5)) * 0.6 + sine 210 * 0.5) * adsr 0.001 0.07 0 0.03 0.12) * 0.7
  "cp" -> onepole 8000 (highpass 1500 (noise 9)) * adsr 0.001 0.03 0 0.02 0.06 * 0.5
  "hh" -> onepole 9000 (highpass 5500 (noise 4)) * adsr 0.0005 0.02 0 0.01 0.035 * 0.3
  _ -> fromSamples []

stutter :: Sig
stutter = play glitchKit (bar (chunk 4 (ply 4) "bd sn cp hh")) * gate 0.01 8

shuffled :: Sig
shuffled = play glitchKit (bar (shuffle 4 "bd sn cp hh")) * gate 0.01 8

grain :: Sig
grain = play glitchKit (bar (someCyclesBy 0.5 (linger (1 / 8)) (scramble 8 "bd hh sn hh cp hh sn hh"))) * gate 0.01 16

loop :: Stereo
loop = bothChannels (\c -> shaper 1.3 (c * gate 0.02 16 * 0.75) * 0.9) mixed
  where
    beat =
      bar
        ( stack
            [ "bd ~ ~ bd ~ ~ ~ ~"
            , whenmod 4 2 (ply 3) "~ ~ sn ~ ~ ~ sn ~"
            , sometimesBy 0.3 (fast 2) "hh*8?0.25"
            , someCyclesBy 0.4 (scramble 8) "~ cp ~ ~ ~ ~ cp ~"
            ]
        )
    kickSig = share (play glitchKit (bar "bd ~ ~ bd ~ ~ ~ ~"))
    duck = sidechain kickSig 0.5
    drums = play glitchKit beat
    pad = duck (play padInst (bar (chunk 4 (linger (1 / 4)) "a2 ~ ~ ~ f2 ~ ~ ~"))) * 1.3
    bass = duck (play bassInst (bar "a1 ~ a1 ~ f1 ~ ~ a1")) * 1.2
    mixed = mixStereo [mono drums, pan (-0.3) pad, mono bass, pan 0.35 (drums * 0.25)]

padInst :: Instrument
padInst n =
  svfBand (constant (noteFreq n * 6) + 400 * sine 0.13) 0.6 (saw (constant (noteFreq n)) * 0.4)
    * adsr 0.05 0.4 0.5 0.6 (max 1 (noteDur n))
    * constant (noteAmp n * 0.5)

bassInst :: Instrument
bassInst n =
  decimate 8 2 (ladder (120 + 400 * expdecay 0.06) 0.6 (saw (constant (noteFreq n)) * 0.5))
    * adsr 0.005 0.12 0.4 0.06 (noteDur n * 0.9)
    * constant (noteAmp n * 0.5)

examples :: [Example]
examples =
  [ example "17-stutter" stutter
  , example "17-shuffled" shuffled
  , example "17-grain" grain
  , exampleWide "17-idm" loop
  ]
