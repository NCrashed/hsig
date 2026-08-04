-- | Конечное распределение: нормировка, энтропия, байес, расхождения.
module DistSpec (tests) where

import Sound.Pred.Dist
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Dist"
    [ buildTests
    , entropyTests
    , mixTests
    , bayesTests
    , divergenceTests
    , sampleTests
    , pruneTests
    ]

-- Энтропия H(2/3, 1/3) в битах: встречается дальше в инвариантах машин.
h23 :: Double
h23 = 0.9182958340544896

eps :: Double
eps = 1e-12

near :: String -> Double -> Double -> Assertion
near what expected actual =
  assertBool
    (what <> ": ожидалось " <> show expected <> ", получено " <> show actual)
    (abs (expected - actual) < 1e-9)

buildTests :: TestTree
buildTests =
  testGroup
    "построение"
    [ testCase "веса нормируются" $ do
        near "сумма" 1 (sum (map snd (distPairs (dist [('a', 3), ('b', 1)]))))
    , testCase "одинаковые исходы склеиваются" $ do
        distPairs (dist [('a', 1), ('a', 1), ('b', 2)])
          @?= distPairs (dist [('a', 1), ('b', 1)])
    , testCase "неположительные веса выбрасываются" $ do
        support (dist [('a', 1), ('b', 0), ('c', -1)]) @?= "a"
    , testCase "порядок входа не влияет" $ do
        distPairs (dist [('b', 1), ('a', 3)]) @?= distPairs (dist [('a', 3), ('b', 1)])
    , testCase "dirac сосредоточен" $ do
        distPairs (dirac 'x') @?= [('x', 1)]
    , testCase "uniform равномерен" $ do
        distPairs (uniform "abcd") @?= [(c, 0.25) | c <- "abcd"]
    , testCase "probOf вне носителя это ноль" $ do
        probOf 'z' (uniform "ab") @?= 0
    , testProperty "сумма весов всегда единица" $ \(NonEmpty xs) ->
        let ws = [(c, abs w + 1) | (c, w) <- xs :: [(Char, Double)]]
         in abs (sum (map snd (distPairs (dist ws))) - 1) < 1e-9
    ]

entropyTests :: TestTree
entropyTests =
  testGroup
    "энтропия"
    [ testCase "dirac даёт ноль" $ do
        near "H" 0 (entropy (dirac 'a'))
    , testCase "монета даёт бит" $ do
        near "H" 1 (entropy (uniform "ab"))
    , testCase "четыре исхода дают два бита" $ do
        near "H" 2 (entropy (uniform "abcd"))
    , testCase "H(2/3, 1/3) это замкнутая форма" $ do
        near "H" h23 (entropy (dist [('a', 2), ('b', 1)]))
    , testCase "сюрприз это минус логарифм вероятности" $ do
        near "I" 2 (surprisalOf (uniform "abcd") 'a')
    , testCase "сюрприз вне носителя бесконечен" $ do
        assertBool "не бесконечность" (isInfinite (surprisalOf (uniform "ab") 'z'))
    , testProperty "энтропия не превосходит логарифма носителя" $ \(NonEmpty xs) ->
        let d = dist [(c, abs w + 1) | (c, w) <- xs :: [(Char, Double)]]
            n = fromIntegral (length (support d))
         in entropy d <= logBase 2 n + 1e-9
    ]

mixTests :: TestTree
mixTests =
  testGroup
    "смесь"
    [ testCase "две дельты дают равномерное" $ do
        distPairs (mix [(1, dirac 'a'), (1, dirac 'b')]) @?= distPairs (uniform "ab")
    , testCase "вес учитывается" $ do
        near "p" 0.75 (probOf 'a' (mix [(3, dirac 'a'), (1, dirac 'b')]))
    , testCase "смесь с одной компонентой это она сама" $ do
        distPairs (mix [(5, uniform "abc")]) @?= distPairs (uniform "abc")
    , testCase "нулевые компоненты игнорируются" $ do
        distPairs (mix [(0, dirac 'a'), (1, dirac 'b')]) @?= distPairs (dirac 'b')
    ]

bayesTests :: TestTree
bayesTests =
  testGroup
    "байес"
    [ testCase "равные приоры дают веса по правдоподобию" $ do
        bayes [(0.5, 0.8), (0.5, 0.2)] @?~ [0.8, 0.2]
    , testCase "приор учитывается" $ do
        -- 0.9*0.1 = 0.09 против 0.1*0.9 = 0.09, ровно поровну.
        bayes [(0.9, 0.1), (0.1, 0.9)] @?~ [0.5, 0.5]
    , testCase "нулевое правдоподобие убивает компоненту" $ do
        bayes [(0.5, 0), (0.5, 1)] @?~ [0, 1]
    , -- Наблюдение вне носителя всех компонент не различает их, поэтому
      -- приоры обязаны остаться нетронутыми, а не превратиться в NaN.
      testCase "невозможное наблюдение оставляет приоры" $ do
        bayes [(0.3, 0), (0.7, 0)] @?~ [0.3, 0.7]
    , testCase "апостериоры нормированы" $ do
        near "сумма" 1 (sum (bayes [(0.2, 3), (0.3, 1), (0.5, 7)]))
    ]
  where
    (@?~) :: [Double] -> [Double] -> Assertion
    xs @?~ ys = do
      length xs @?= length ys
      sequence_ [near "вес" y x | (x, y) <- zip xs ys]

divergenceTests :: TestTree
divergenceTests =
  testGroup
    "расхождения"
    [ testCase "KL от себя это ноль" $ do
        near "KL" 0 (kl (dist [('a', 2), ('b', 1)]) (dist [('a', 2), ('b', 1)]))
    , testCase "KL дельты от равномерного это логарифм носителя" $ do
        near "KL" 2 (kl (dirac 'a') (uniform "abcd"))
    , testCase "KL бесконечен вне носителя знаменателя" $ do
        assertBool "не бесконечность" (isInfinite (kl (dirac 'z') (uniform "ab")))
    , testCase "полная вариация разных дельт это единица" $ do
        near "TV" 1 (totalVariation (dirac 'a') (dirac 'b'))
    , testCase "полная вариация от себя это ноль" $ do
        near "TV" 0 (totalVariation (uniform "abc") (uniform "abc"))
    , testCase "полная вариация половины сдвига" $ do
        -- |0.5-0.25|+|0.5-0.25|+|0-0.25|+|0-0.25| = 1, пополам это 0.5.
        near "TV" 0.5 (totalVariation (uniform "ab") (uniform "abcd"))
    , testProperty "полная вариация симметрична и лежит в [0,1]" $ \(NonEmpty xs) (NonEmpty ys) ->
        let p = dist [(c, abs w + 1) | (c, w) <- xs :: [(Char, Double)]]
            q = dist [(c, abs w + 1) | (c, w) <- ys :: [(Char, Double)]]
            d = totalVariation p q
         in abs (d - totalVariation q p) < 1e-9 && d >= -1e-9 && d <= 1 + 1e-9
    ]

sampleTests :: TestTree
sampleTests =
  testGroup
    "выборка"
    [ testCase "ноль берёт первый исход" $ do
        sampleWith 0 (uniform "abc") @?= 'a'
    , testCase "почти единица берёт последний" $ do
        sampleWith (1 - eps) (uniform "abc") @?= 'c'
    , testCase "середина попадает по весам" $ do
        sampleWith 0.5 (dist [('a', 3), ('b', 1)]) @?= 'a'
    , testCase "выход за единицу не падает" $ do
        sampleWith 1.5 (uniform "abc") @?= 'c'
    , testProperty "частоты сходятся к весам" $ once $
        let d = dist [('a', 3), ('b', 1)]
            n = 20000 :: Int
            us = [(fromIntegral i + 0.5) / fromIntegral n | i <- [0 .. n - 1]]
            freq :: Char -> Double
            freq c = fromIntegral (length (filter (== c) (map (`sampleWith` d) us))) / fromIntegral n
         in abs (freq 'a' - 0.75) < 1e-3
    ]

pruneTests :: TestTree
pruneTests =
  testGroup
    "прореживание"
    [ testCase "оставляет тяжёлые" $ do
        support (prune 2 (dist [('a', 5), ('b', 3), ('c', 1)])) @?= "ab"
    , testCase "перенормирует остаток" $ do
        near "p" 0.625 (probOf 'a' (prune 2 (dist [('a', 5), ('b', 3), ('c', 1)])))
    , testCase "запас больше носителя ничего не меняет" $ do
        distPairs (prune 10 (uniform "abc")) @?= distPairs (uniform "abc")
    ]
