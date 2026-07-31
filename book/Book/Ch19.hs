-- | Код главы 19: измеренные HRTF.
--
-- Единственная глава, которой нужны данные снаружи: набор KEMAR приезжает
-- через flake, путь берётся из HSIG_HRTF.
module Book.Ch19
  ( examples
  , source
  , spin
  , frontBack
  ) where

import Book.Prelude
import Sound.Sig
import Sound.Sig.HRTF

-- | Источник с широким спектром: направление слышно по спектральным
-- признакам, поэтому синус для этого не годится.
source :: Double -> Sig
source secs = share (play voice (fast 2 "a3 c4 e4 g4") * gate 0.01 secs)
  where
    voice n =
      (saw (constant (noteFreq n)) * 0.16 + highpass 3000 (noise 3) * 0.11)
        * adsr 0.002 0.15 0.3 0.15 (noteDur n * 0.9)

-- | Оборот вокруг головы за восемь секунд.
spin :: Hrtf -> Stereo
spin h = binaural h angle (source 8)
  where
    angle = 2 * pi * line [(0, 0), (8, 1)]

-- | Четыре секунды спереди, четыре сзади: сравнение без движения.
frontBack :: Hrtf -> Stereo
frontBack h = binaural h angle (source 8)
  where
    angle = constant pi * line [(0, 0), (3.9, 0), (4, 1), (8, 1)]

examples :: Hrtf -> [Example]
examples h =
  [ exampleWide "19-hrtf-spin" (spin h)
  , exampleWide "19-hrtf-frontback" (frontBack h)
  , exampleWide "19-orbit-spin" (orbit (2 * pi * line [(0, 0), (8, 1)]) (source 8))
  ]
