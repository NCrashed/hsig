-- | Бисимуляционная псевдометрика.
module MetricSpec (tests) where

import Sound.Pred.Dist
import Sound.Pred.Kernel
import Sound.Pred.Machine
import Sound.Pred.Metric
import Sound.Pred.Model
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Metric"
    [ axiomTests
    , behaviourTests
    , orderTests
    ]

near :: String -> Double -> Double -> Assertion
near what expected actual =
  assertBool
    (what <> ": ожидалось " <> show expected <> ", получено " <> show actual)
    (abs (expected - actual) < 1e-9)

biased :: Double -> Pred Char
biased p = constPred (dist [('a', p), ('b', 1 - p)])

-- | Набор разнородных моделей для проверки аксиом на всех парах и тройках.
zoo :: [Pred Int]
zoo =
  [ constPred (uniform [0, 1])
  , constPred (dirac 0)
  , constPred (dirac 1)
  , constPred (dist [(0, 0.9), (1, 0.1)])
  , toPred (evenProcess 0.5)
  , toPred (goldenMean 0.5)
  , toPred (evenProcess 0.9)
  ]

axiomTests :: TestTree
axiomTests =
  testGroup
    "аксиомы"
    [ testCase "расстояние до себя нулевое" $ do
        sequence_ [near "d(s,s)" 0 (bisimDist m m) | m <- zoo]
    , testCase "симметрия" $ do
        sequence_
          [ near "d(s,t) - d(t,s)" (bisimDist s t) (bisimDist t s)
          | s <- zoo
          , t <- zoo
          ]
    , testCase "неравенство треугольника" $ do
        sequence_
          [ assertBool
            ("треугольник нарушен: " <> show (bisimDist s u, bisimDist s t, bisimDist t u))
            (bisimDist s u <= bisimDist s t + bisimDist t u + 1e-9)
          | s <- zoo
          , t <- zoo
          , u <- zoo
          ]
    , testCase "значения лежат в [0, 1]" $ do
        sequence_
          [ assertBool ("вне отрезка: " <> show d) (d >= 0 && d <= 1)
          | s <- zoo
          , t <- zoo
          , let d = bisimDist s t
          ]
    ]

behaviourTests :: TestTree
behaviourTests =
  testGroup
    "поведение, а не синтаксис"
    [ -- Ключевой тест коалгебры: модели собраны по-разному, ведут себя
      -- одинаково, значит неразличимы. Алгебраическое сравнение термов
      -- дало бы тут «разные».
      testCase "разная конструкция при одинаковом поведении даёт ноль" $ do
        let byConst = constPred (uniform "ab")
            byUnfold = unfoldPred (const (uniform "ab")) (\s _ -> s + 1) (0 :: Int)
        near "d" 0 (bisimDist byConst byUnfold)
    , testCase "машина и её развёртка неразличимы" $ do
        let m = goldenMean 0.6
            copy = unfoldPred (machineOut m) (machineStep m) (machineStart m)
        near "d" 0 (bisimDist (toPred m) copy)
    , testCase "разные дельты отстоят на единицу" $ do
        near "d" 1 (bisimDist (constPred (dirac 'a')) (constPred (dirac 'b')))
    , -- Одна и та же топология, разная разметка выходов: инварианты
      -- совпадают (см. MachineSpec), а поведение различается, и метрика
      -- обязана это видеть.
      testCase "even и golden mean различимы" $ do
        assertBool
          "метрика не различает процессы"
          (bisimDist (toPred (evenProcess 0.5)) (toPred (goldenMean 0.5)) > 0.1)
    ]

orderTests :: TestTree
orderTests =
  testGroup
    "порядок"
    [ testCase "близкие смещения ближе далёких" $ do
        let d1 = bisimDist (biased 0.9) (biased 0.8)
            d2 = bisimDist (biased 0.9) (biased 0.1)
        assertBool (show (d1, d2)) (d1 < d2)
    , testCase "полная вариация это нижняя граница" $ do
        sequence_
          [ assertBool "метрика меньше полной вариации сейчас" (bisimDist s t + 1e-9 >= tv)
          | s <- zoo
          , t <- zoo
          , let tv = totalVariation (predict s) (predict t)
          ]
    , testCase "матрица симметрична с нулевой диагональю" $ do
        let m = distMatrix zoo
            n = length zoo
        sequence_ [near "диагональ" 0 (m !! i !! i) | i <- [0 .. n - 1]]
        sequence_ [near "симметрия" (m !! i !! j) (m !! j !! i) | i <- [0 .. n - 1], j <- [0 .. n - 1]]
    ]
