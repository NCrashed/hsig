-- | Код главы 11: техно.
module Book.Ch11
  ( examples
  , stab
  , riser
  , loop
  ) where

import Book.Ch06 (kit)
import Book.Prelude
import Sound.Sig

-- | Аккорд-стаб: три пилы через один фильтр, огибающая короткая, весь
-- характер в резонансе.
stab :: Instrument
stab n =
  ladder cut 0.85 (mix [saw (constant (noteFreq n * k)) * 0.3 | k <- [1, 1.5, 2]])
    * adsr 0.003 0.09 0.15 0.06 (noteDur n * 0.8)
    * constant (noteAmp n * 0.35)
  where
    cut = 400 + 900 * expdecay 0.05

-- | Подъём: шум в узкой полосе, которая едет вверх шестнадцать секунд.
riser :: Sig
riser = svfBand cut 0.9 (noise 7) * line [(0, 0), (16, 0.5)] * 0.6
  where
    cut = 300 * exp (line [(0, 0), (16, log 20)])

loop :: Stereo
loop = bothChannels (\c -> shaper 1.2 (c * gate 0.02 16 * 0.9)) mixed
  where
    bar = slow 2
    kickSig = share (play kit (bar "bd*4"))
    duck = sidechain kickSig 0.85
    drums = play kit (bar (stack ["bd*4", "[~ hh]*4", "~ ~ ~ sn?0.7"]))
    chords = duck (play stab (bar (every 8 (fast 2) "~ a3 ~ [c4 e4]"))) * 1.4
    sweep = ladder cutSweep 0.8 (saw 55 * 0.35) * gate 0.02 16
    cutSweep = 90 * exp (line [(0, 0), (8, log 30), (16, log 3)])
    mixed = mixStereo [mono drums, mono (duck sweep * 1.2), pan 0.3 chords, pan (-0.2) (duck riser)]

examples :: [Example]
examples = [exampleWide "11-techno" loop]
