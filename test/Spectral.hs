-- | Спектральные хелперы для тестов.
module Spectral
  ( spectrum
  , rms
  ) where

import Data.Complex (Complex, magnitude)
import Data.Vector.Storable qualified as V
import Data.Vector.Unboxed qualified as U
import Numeric.FFT.Vector.Unnormalized qualified as FFT

-- | Модуль спектра, ненормированный R2C.
spectrum :: U.Vector Double -> [Double]
spectrum v = map magnitude (V.toList spec)
  where
    spec = FFT.run FFT.dftR2C (V.fromList (U.toList v)) :: V.Vector (Complex Double)

rms :: U.Vector Double -> Double
rms v
  | U.null v = 0
  | otherwise = sqrt (U.sum (U.map (\x -> x * x) v) / fromIntegral (U.length v))
