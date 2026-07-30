-- | Код главы 14: два тембра, колокол и текстурный бас.
module Book.Ch14
  ( examples
  , bell
  , bellInst
  , bellHeavy
  , bells
  , steps
  , bubble
  , bubbles
  , subLayer
  , gritLayer
  , airLayer
  , lornBass
  , lornLoop
  ) where

import Book.Ch06 (kit)
import Book.Prelude
import Sound.Sig

bell :: Double -> Double -> Sig
bell f dur = mix [partial r a d | (r, a, d) <- voices] * gate 0.002 dur * 0.3
  where
    partial r a d = sine (constant (f * r)) * expdecay (dur * d) * constant (a / loudness)
    loudness = sum [a | (_, a, _) <- voices]
    voices =
      [ (0.56, 1.00, 1.00)
      , (0.92, 0.67, 0.90)
      , (1.19, 1.00, 0.65)
      , (1.70, 1.80, 0.55)
      , (2.00, 2.67, 0.33)
      , (2.74, 1.67, 0.35)
      , (3.00, 1.46, 0.25)
      , (3.76, 1.33, 0.20)
      , (4.07, 1.33, 0.15)
      ]

bellInst :: Instrument
bellInst n = bell (noteFreq n) 3.5 * constant (noteAmp n)

bellHeavy :: Double -> Double -> Sig
bellHeavy f dur = oversample 4 (shaper 5) (bell f dur * 1.8) * 0.55

steps :: Int -> Int -> Sig
steps seed hold = decimate 24 hold (noise seed)

bubble :: Instrument
bubble n =
  ladder cut 0.97 (pulse 0.18 (constant f) * 0.4)
    * adsr 0.001 0.08 0 0.03 (noteDur n)
    * constant (noteAmp n * 0.7)
  where
    f = noteFreq n
    cut = constant f * 4 * exp (1.1 * steps 5 1800)

bubbles :: Sig
bubbles = play bubble (slow 2 "[c3 g3 c4 eb3]*4?0.3") * gate 0.01 8

bells :: Sig
bells = play bellInst (slow 4 "e5 b4 e5 c5 e5 d5 e5 [b4 a4]") * gate 0.02 16

subLayer :: Double -> Sig
subLayer f = sine (constant f) * adsr 0.02 0.1 0.9 0.15 1.8 * 0.55

gritLayer :: Double -> Sig
gritLayer f =
  decimate 6 3 (oversample 4 (shaper 9) (ladder cut 0.55 (saw (constant f) * 0.45)))
    * adsr 0.03 0.2 0.7 0.2 1.8
    * 0.3
  where
    cut = 90 + 260 * (0.5 + 0.5 * sine 0.23)

airLayer :: Sig
airLayer = svfBand (500 + 380 * sine 0.11) 0.85 (noise 11) * adsr 0.4 0.5 0.6 0.5 1.8 * 0.25

lornBass :: Instrument
lornBass n =
  compress 0.25 6 0.02 0.25 (subLayer f + gritLayer f + airLayer)
    * constant (noteAmp n * 0.8)
  where
    f = noteFreq n

lornLoop :: Stereo
lornLoop = bothChannels (\c -> shaper 1.4 (c * gate 0.02 16 * 0.85)) mixed
  where
    bar = slow (240 / 130)
    kickSig = share (play kit (bar "bd ~ ~ ~ bd ~ ~ ~") * 1.2)
    duck = sidechain kickSig 0.6
    drums = play kit (bar (stack ["bd ~ ~ ~ bd ~ ~ ~", "~ ~ ~ ~ sn ~ ~ ~", "hh*4?0.4"]))
    bassPart = duck (play lornBass (bar "c1 ~ ~ ~ ~ ~ eb1 ~")) * 1.3
    mixed = mixStereo [mono drums, mono bassPart]

examples :: [Example]
examples =
  [ example "14-bell" (bell 440 3.5)
  , example "14-bell-heavy" (bellHeavy 440 3.5)
  , example "14-bells" (bells * 1.5)
  , example "14-bubbles" (bubbles * 1.5)
  , example "14-bass-sub" (subLayer 41.2 * gate 0.02 1.8)
  , example "14-bass-grit" (gritLayer 41.2 * gate 0.02 1.8)
  , example "14-bass-full" (play lornBass (slow 2 "c1 ~ eb1 ~") * gate 0.02 8)
  , exampleWide "14-lorn" lornLoop
  ]
