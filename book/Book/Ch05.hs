-- | Код главы 5: нелинейности и оверсэмплинг.
module Book.Ch05
  ( examples
  , driven
  , soft
  , hard
  , crushed
  ) where

import Book.Prelude
import Sound.Sig

driven :: Fx -> Sig
driven fx = svf 8000 0.3 (fx (saw 1493 * 0.45)) * 0.5 * gate 0.01 2

soft :: Sig
soft = shaper 6 (saw 110 * 0.4) * gate 0.01 2

hard :: Sig
hard = clip 0.15 (saw 110 * 0.4) * gate 0.01 2

crushed :: Sig
crushed = decimate 4 8 (saw 110 * 0.4) * gate 0.01 2

examples :: [Example]
examples =
  [ example "05-drive-plain" (driven (shaper 6))
  , example "05-drive-oversampled" (driven (oversample 8 (shaper 6)))
  , example "05-soft" soft
  , example "05-hard" hard
  , example "05-crushed" crushed
  ]
