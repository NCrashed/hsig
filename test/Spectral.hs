-- | Спектральные хелперы для тестов.
module Spectral
  ( spectrum
  , blackman
  , windowed
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

-- | Окно Блэкмана: боковые лепестки около -58 дБ и быстрый спад.
blackman :: Int -> U.Vector Double
blackman n = U.generate n $ \i ->
  let t = 2 * pi * fromIntegral i / fromIntegral (n - 1)
   in 0.42 - 0.5 * cos t + 0.08 * cos (2 * t)

-- | Спектр с окном. Без окна утечка от компонент у Найквиста заливает весь
-- спектр и меряться будет она, а не сигнал.
windowed :: U.Vector Double -> [Double]
windowed v = spectrum (U.zipWith (*) (blackman (U.length v)) v)

rms :: U.Vector Double -> Double
rms v
  | U.null v = 0
  | otherwise = sqrt (U.sum (U.map (\x -> x * x) v) / fromIntegral (U.length v))
