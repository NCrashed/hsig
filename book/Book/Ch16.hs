-- | Код главы 16: даунтемпо.
module Book.Ch16
  ( examples
  , bar
  , crackle
  , rhodes
  , warmBass
  , straight
  , swung
  , loop
  ) where

import Book.Ch06 (kit)
import Book.Prelude
import Sound.Sig

-- | 86 ударов в минуту: цикл это четыре доли, значит его длина 240\/86.
bar :: Pattern a -> Pattern a
bar = slow (240 / 86)

crackle :: Sig
crackle = onepole 4000 (mapSig spark (noise 21)) * 0.5
  where
    spark v = if abs v > 0.985 then signum v * (abs v - 0.985) * 60 else 0

rhodes :: Instrument
rhodes n =
  shaper 1.6 (mix [tine 1 1 0.35, tine 2 0.45 0.12, tine 4.02 0.2 0.05])
    * adsr 0.004 0.35 0.45 0.5 (max 0.6 (noteDur n))
    * constant (noteAmp n * 0.45)
  where
    f = noteFreq n
    tine ratio amp decay = sine (constant (f * ratio)) * expdecay decay * amp

warmBass :: Instrument
warmBass n =
  onepole 320 (shaper 2.5 (sine (constant (noteFreq n)) * 0.8))
    * adsr 0.02 0.15 0.75 0.2 (noteDur n * 0.95)
    * constant (noteAmp n * 0.55)

straight :: Sig
straight = play kit (bar (stack ["bd ~ ~ ~ sn ~ ~ ~", "hh*8"])) * gate 0.01 8

swung :: Sig
swung = play kit (swingBy (1 / 5) 4 (bar (stack ["bd ~ ~ ~ sn ~ ~ ~", "hh*8?0.2"]))) * gate 0.01 8

loop :: Stereo
loop = bothChannels (\c -> shaper 1.2 (c * gate 0.05 24 * 0.55)) mixed
  where
    drums = swingBy (1 / 5) 4 (bar (stack ["bd ~ ~ ~ sn ~ ~ ~", "hh*8?0.2", "~ ~ ~ ~ ~ ~ bd ~"]))
    kickSig = share (play kit (bar "bd ~ ~ ~ ~ ~ ~ ~"))
    duck = sidechain kickSig 0.35
    keys = duck (play rhodes (bar (off (1 / 8) (fmap quieter) "a3 ~ c4 ~ ~ e4 ~ ~"))) * 1.2
    bass = duck (play warmBass (bar "a1 ~ ~ ~ f1 ~ ~ ~")) * 1.4
    quieter n = n {noteAmp = noteAmp n * 0.4, noteFreq = noteFreq n * 2}
    mixed =
      mixStereo
        [ mono (play kit drums * 0.9)
        , pan (-0.25) keys
        , mono bass
        , pan 0.3 (crackle * 0.5 * gate 0.05 24)
        ]

examples :: [Example]
examples =
  [ example "16-straight" straight
  , example "16-swung" swung
  , example "16-crackle" (crackle * gate 0.05 6)
  , example "16-rhodes" (play rhodes (bar "a3 ~ c4 e4") * gate 0.01 8)
  , exampleWide "16-downtempo" loop
  ]
