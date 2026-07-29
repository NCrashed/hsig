-- | Индексируемый ГПСЧ: сверка с эталоном, детерминизм, распределение.
module RandomSpec (tests) where

import Data.List (sort)
import Sound.Sig.Random
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Random"
    [ -- Первые выходы splitmix64 при seed 0: индексируемая обёртка обязана
      -- совпадать с эталонным алгоритмом, иначе это другой генератор.
      testCase "совпадает с эталонным splitmix64" $ do
        map (wordAt 0) [0, 1, 2]
          @?= [0xE220A8397B1DCDAF, 0x6E789E6AA1B965F4, 0x06C45D188009454F]
    , testProperty "разные индексы дают разные значения" $ \s (NonNegative i) (NonNegative j) ->
        i /= j ==> wordAt s i =/= wordAt s j
    , testProperty "значения лежат в [0, 1)" $ \s i ->
        let x = doubleAt s i in x >= 0 .&&. x < 1
    , testCase "поток без повторов на длинном отрезке" $ do
        let xs = sort (map (wordAt 0) [0 .. 9999])
        length (filter id (zipWith (==) xs (drop 1 xs))) @?= 0
    , testCase "разные seed дают разные потоки" $ do
        let a = map (doubleAt 0) [0 .. 999]
            b = map (doubleAt 1) [0 .. 999]
        length (filter id (zipWith (==) a b)) @?= 0
    , testCase "среднее около 0.5" $ do
        let n = 100000
            mean = sum (map (doubleAt 7) [0 .. n - 1]) / fromIntegral n
        assertBool (show mean) (abs (mean - 0.5) < 0.01)
    ]
