-- | Пока только проверки окружения. Содержательные тесты идут с M0-M7.
module Main
  ( main
  ) where

import Data.Complex (Complex, magnitude)
import Data.Vector.Storable qualified as V
import Data.Vector.Unboxed qualified as U
import Numeric.FFT.Vector.Unnormalized qualified as FFT
import Sound.Sig (version)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "hsig"
    [ testGroup
        "окружение"
        [ testCase "библиотека линкуется" $
            version @?= "0.1.0.0"
        , testCase "vector-fftw считает спектр" fftSmoke
        , testProperty "unboxed-векторы round-trip" $
            \(xs :: [Double]) -> U.toList (U.fromList xs) === xs
        ]
    ]

-- | Период косинуса на 64 точках: вся энергия в бине 1, амплитуда n/2
-- (преобразование ненормированное). Заодно проверяет линковку с FFTW.
fftSmoke :: Assertion
fftSmoke = do
  let n = 64 :: Int
      xs = V.generate n $ \i ->
        cos (2 * pi * fromIntegral i / fromIntegral n)
      spectrum = FFT.run FFT.dftR2C xs :: V.Vector (Complex Double)
      mags = V.toList (V.map magnitude spectrum)

  length mags @?= n `div` 2 + 1
  assertBool "бин 1 несёт n/2" (abs (mags !! 1 - fromIntegral n / 2) < 1e-9)
  sequence_
    [ assertBool ("бин " ++ show k ++ " пуст") (m < 1e-9)
    | (k, m) <- zip [0 :: Int ..] mags
    , k /= 1
    ]
