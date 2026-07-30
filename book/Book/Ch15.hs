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

examples :: [Example]
examples =
  [ example "15-euclid" euclidKit
  , example "15-notation" notated
  , example "15-off" echoed
  , example "15-ply" plied
  , example "15-sometimes" varied
  , example "15-struct" structured
  ]
