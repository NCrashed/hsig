-- | Код главы 12: эмбиент.
module Book.Ch12
  ( examples
  , pad
  , reverb
  , loop
  , dryPad
  ) where

import Book.Prelude
import Sound.Sig

-- | Пэд: расстроенный унисон под одноплюсником, атака в полсекунды.
pad :: Double -> Double -> Sig
pad f dur =
  onepole (700 + 400 * sine 0.07) (mix [voice c | c <- [-7, -2, 3, 9]])
    * adsr (dur * 0.35) (dur * 0.2) 0.7 (dur * 0.45) dur
    * 0.2
  where
    voice cents = saw (constant (f * 2 ** (cents / 1200))) * 0.5

-- | Реверб по Шрёдеру: гребёнки создают плотность, оллпассы размазывают
-- их резонансы. Времена взаимно непростые, иначе отражения складываются в
-- слышимый тон.
reverb :: Sig -> Sig
reverb src = allpass 0.005 0.7 (allpass 0.017 0.7 (mix [comb t 0.82 src * 0.25 | t <- times]))
  where
    times = [0.0297, 0.0371, 0.0411, 0.0437]

loop :: Stereo
loop = bothChannels (* gate 0.05 20) (mixStereo [orbit angle wet, mono (dry * 0.5)])
  where
    dry = share (mix [delay t (pad f 12) | (t, f) <- [(0, 110), (3, 164.81), (6, 130.81), (9, 220)]])
    wet = reverb dry * 0.9
    angle = 2 * pi * 0.05 * line [(0, 0), (20, 20)]

dryPad :: Sig
dryPad = mix [delay t (pad f 12) | (t, f) <- [(0, 110), (3, 164.81)]] * gate 0.05 18

examples :: [Example]
examples =
  [ exampleWide "12-ambient" loop
  , example "12-dry" dryPad
  ]
