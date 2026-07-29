-- | Индексируемый ГПСЧ splitmix64.
--
-- Значение зависит только от seed и номера сэмпла, поэтому рендер
-- детерминирован, а блоки можно считать в любом порядке и параллельно.
module Sound.Sig.Random
  ( wordAt
  , doubleAt
  ) where

import Data.Bits (shiftR, xor)
import Data.Word (Word64)

-- | Шаг счётчика splitmix64, нечётный, поэтому индексация биективна.
golden :: Word64
golden = 0x9E3779B97F4A7C15

-- | Финализатор splitmix64.
mix :: Word64 -> Word64
mix z0 = z2 `xor` (z2 `shiftR` 31)
  where
    z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
    z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB

wordAt :: Int -> Int -> Word64
wordAt seed i = mix (fromIntegral seed + golden * (fromIntegral i + 1))

-- | Равномерное в [0, 1) из старших 53 бит.
doubleAt :: Int -> Int -> Double
doubleAt seed i = fromIntegral (wordAt seed i `shiftR` 11) * 0x1p-53
