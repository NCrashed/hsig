-- | Код главы 13: драм-н-бэйс.
module Book.Ch13
  ( examples
  , bar
  , reese
  , sub
  , loop
  ) where

import Book.Ch06 (kit)
import Book.Prelude
import Sound.Sig

-- | 174 удара в минуту. Цикл это четыре доли, значит его длина 240\/174.
bar :: Pattern a -> Pattern a
bar = slow (240 / 174)

-- | Reese: две пилы, расстроенные на доли герца. Биения между ними и есть
-- то самое движение; фильтр сверху их подчёркивает.
reese :: Note -> Stereo
reese n = Stereo (voice (-0.4)) (voice 0.4)
  where
    voice d =
      ladder cut 0.7 (mix [saw (constant (noteFreq n + d + s)) * 0.35 | s <- [0, 7]])
        * adsr 0.01 0.1 0.85 0.08 (noteDur n * 0.95)
        * constant (noteAmp n * 0.35)
    cut = 200 + 700 * (0.5 + 0.5 * sine 0.35)

sub :: Instrument
sub n = sine (constant (noteFreq n)) * adsr 0.01 0.05 0.9 0.05 (noteDur n * 0.95) * 0.5

loop :: Stereo
loop = bothChannels (\c -> shaper 1.3 (c * gate 0.02 11 * 0.9)) mixed
  where
    kickSig = share (play kit (bar "bd ~ ~ bd ~ ~ ~ ~"))
    duck = sidechain kickSig 0.5
    break2 = play kit (bar (stack ["bd ~ ~ bd ~ ~ ~ ~", "~ ~ sn ~ ~ ~ sn ~"]))
    bassLine = bothChannels (\c -> duck c * 1.5) (playStereo reese (bar "a1 ~ ~ a1 c2 ~ a1 ~"))
    subLine = duck (play sub (bar "a1 ~ ~ ~ c2 ~ ~ ~")) * 1.2
    hats = play kit (bar "hh*8?0.3")
    mixed = mixStereo [mono break2, pan 0.35 hats, bassLine, mono subLine]

examples :: [Example]
examples = [exampleWide "13-dnb" loop]
