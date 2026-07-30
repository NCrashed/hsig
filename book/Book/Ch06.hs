-- | Код главы 6: партитура и мини-нотация.
module Book.Ch06
  ( examples
  , kit
  , lead
  , beat
  , melody
  , sparse
  ) where

import Book.Prelude
import Sound.Sig

kit :: Instrument
kit n = case noteLabel n of
  "bd" -> sine (55 * (1 + 6 * expdecay 0.02)) * adsr 0.001 0.15 0 0.02 0.2 * 0.5
  "sn" -> (highpass 1200 (noise 3) * 0.5 + sine 190 * 0.4) * adsr 0.001 0.1 0 0.05 0.18 * 0.45
  "hh" -> highpass 7000 (noise 2) * adsr 0.001 0.04 0 0.01 0.06 * 0.25
  _ -> fromSamples []

lead :: Instrument
lead n =
  ladder (300 + 3000 * expdecay 0.08) 0.6 (saw (constant (noteFreq n)) * 0.4)
    * adsr 0.005 0.1 0.4 0.08 (noteDur n)
    * constant (noteAmp n)

beat :: Sig
beat = play kit (stack ["bd*4", "~ sn ~ sn", "hh*8"]) * gate 0.01 4

melody :: Sig
melody = play lead (slow 2 (every 4 rev "a4 c5 ~ e5")) * gate 0.01 8

sparse :: Sig
sparse = play kit (stack ["bd*4", "hh*16?"]) * gate 0.01 4

examples :: [Example]
examples =
  [ example "06-beat" beat
  , example "06-melody" melody
  , example "06-sparse" sparse
  ]
