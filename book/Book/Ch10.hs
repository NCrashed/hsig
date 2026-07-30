-- | Код главы 10: электро.
module Book.Ch10
  ( examples
  , bar
  , bassInst
  , loop
  ) where

import Book.Ch06 (kit)
import Book.Prelude
import Lead (leadWide)
import Sound.Sig

bar :: Pattern a -> Pattern a
bar = slow 2

bassInst :: Instrument
bassInst n =
  ladder (180 + 2200 * expdecay 0.07) 0.75 (saw (constant (noteFreq n)) * 0.5)
    * adsr 0.005 0.09 0.5 0.05 (noteDur n * 0.9)
    * constant (noteAmp n * 0.5)

loop :: Stereo
loop = bothChannels (\c -> shaper 1.3 (c * window * 0.9)) mixed
  where
    window = gate 0.01 8
    kickSig = share (play kit (bar "bd*4"))
    duck = sidechain kickSig 0.7
    drums = play kit (bar (stack ["bd*4", "~ sn ~ sn", "hh*8"]))
    bassPart = duck (play bassInst (bar "a1 a1 c2 a1")) * 1.8
    lead = bothChannels (\c -> duck c * 0.7) (playStereo leadWide (bar (every 4 rev "a4 c5 ~ e5")))
    mixed = mixStereo [mono drums, mono bassPart, lead]

examples :: [Example]
examples = [exampleWide "10-electro" loop]
