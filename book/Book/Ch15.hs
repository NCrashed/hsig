-- | Код главы 15: комбинаторы паттернов.
module Book.Ch15
  ( examples
  , octaveUp
  , euclidKit
  , echoed
  , plied
  , varied
  , structured
  , notated
  , organ
  , slowBar
  , pulse4
  , onBeat
  , offBeat
  , pushed
  ) where

import Book.Ch06 (kit, lead)
import Book.Prelude
import Sound.Sig

octaveUp :: Pattern Note -> Pattern Note
octaveUp = fmap (\n -> n {noteFreq = noteFreq n * 2, noteAmp = noteAmp n * 0.5})

euclidKit :: Sig
euclidKit =
  play
    kit
    ( stack
        [ euclid 3 8 "bd"
        , euclid 5 8 "hh"
        , slow 2 (euclid 2 5 "sn")
        ]
    )
    * gate 0.01 8

echoed :: Sig
echoed = play lead (off (1 / 8) octaveUp (slow 2 "a3 ~ c4 e4")) * gate 0.01 8

plied :: Sig
plied = play kit (stack ["bd*2", ply 3 "hh hh ~ hh"]) * gate 0.01 8

varied :: Sig
varied =
  play lead (sometimesBy 0.4 (fast 2) (iter 4 "a3 c4 e4 g4")) * gate 0.01 8

structured :: Sig
structured = play lead (struct (euclid 5 8 (pure True)) (slow 2 "a3 c4 e4")) * gate 0.01 8

notated :: Sig
notated = play kit (stack ["bd(3,8)", "~ sn@3", "hh!3 . hh"]) * gate 0.01 8

-- | Орган: синусы по регистрам, ровное тело, мягкие края. Для слышимости
-- смещения важна именно тянущаяся нота: у щипка ухо цепляется за атаку, а
-- тут слышно, где нота стоит.
organ :: Instrument
organ n =
  mix [sine (constant (noteFreq n * r)) * constant a | (r, a) <- draws]
    * adsr 0.03 0.06 0.85 0.12 (noteDur n * 0.95)
    * constant (noteAmp n * 0.16)
  where
    draws = [(0.5, 0.5), (1, 1), (1.5, 0.35), (2, 0.5), (3, 0.2), (4, 0.15)]

-- | 80 ударов в минуту: на быстром темпе смещение на восьмую просто не
-- успеть услышать.
slowBar :: Pattern a -> Pattern a
slowBar = slow 3

-- | Опора: ровные четверти, относительно которых слышно смещение.
pulse4 :: Sig
pulse4 = play kit (slowBar "bd*4") * 0.7

onBeat :: Sig
onBeat = (pulse4 + play organ (slowBar "a3 c4 e4 g4")) * gate 0.01 12

offBeat :: Sig
offBeat = (pulse4 + play organ (slowBar (rotR (1 / 8) "a3 c4 e4 g4"))) * gate 0.01 12

pushed :: Sig
pushed = (pulse4 + play organ (slowBar "~ a3 ~ c4 ~ e4 ~ g4")) * gate 0.01 12

examples :: [Example]
examples =
  [ example "15-euclid" euclidKit
  , example "15-notation" notated
  , example "15-onbeat" onBeat
  , example "15-offbeat" offBeat
  , example "15-pushed" pushed
  , example "15-off" echoed
  , example "15-ply" plied
  , example "15-sometimes" varied
  , example "15-struct" structured
  ]
