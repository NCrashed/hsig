-- | Код главы 1: первый звук.
module Book.Ch01
  ( examples
  , tone
  , clicky
  , chord
  ) where

import Book.Prelude
import Sound.Sig

tone :: Sig
tone = sine 440 * 0.3 * gate 0.01 2

clicky :: Sig
clicky = takeSec 2 (sine 440 * 0.3)

chord :: Sig
chord = mix [voice f d | (f, d) <- [(220, 2), (277.18, 1.4), (330, 0.8)]]
  where
    voice f d = sine (constant f) * 0.2 * gate 0.01 d

examples :: [Example]
examples =
  [ example "01-tone" tone
  , example "01-click" clicky
  , example "01-chord" chord
  ]
