-- | Пространство тембров и общая укладка.
--
-- Проверяется то же, что у аккордов, и теми же словами: метрика это
-- метрика, укладка взаимно однозначна, близкие состояния садятся рядом.
-- Требование не изменилось от смены пространства - в этом и был смысл
-- обобщения.
module TimbreSpec (tests) where

import Data.List (nub)
import Sound.Pred.Embed
import Sound.Pred.Timbre
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Timbre"
    [ metricTests
    , gridTests
    , embedTests
    ]

near :: String -> Double -> Double -> Assertion
near what expected actual =
  assertBool
    (what <> ": ожидалось " <> show expected <> ", получено " <> show actual)
    (abs (expected - actual) < 1e-9)

metricTests :: TestTree
metricTests =
  testGroup
    "метрика"
    [ testCase "расстояние до себя нулевое" $ do
        sequence_ [near "d" 0 (timbreDist t t) | t <- take 40 timbreGrid]
    , testCase "симметрия" $ do
        sequence_
          [ near "d" (timbreDist a b) (timbreDist b a)
          | a <- take 12 timbreGrid
          , b <- take 12 timbreGrid
          ]
    , testCase "неравенство треугольника" $ do
        sequence_
          [ assertBool ("треугольник нарушен: " <> show (a, b, c)) $
            timbreDist a c <= timbreDist a b + timbreDist b c + 1e-9
          | a <- take 8 timbreGrid
          , b <- take 8 timbreGrid
          , c <- take 8 timbreGrid
          ]
    , -- Оси приведены к сопоставимым шкалам намеренно: октава яркости, весь
      -- размах шумности и весь размах резкости должны весить примерно
      -- одинаково, иначе укладка вырождается в одну ось.
      testCase "оси сопоставимы по весу" $ do
        let octave = timbreDist (Timbre 0 3 3) (Timbre 2 3 3)
            allNoise = timbreDist (Timbre 6 0 3) (Timbre 6 6 3)
            allBite = timbreDist (Timbre 6 3 0) (Timbre 6 3 6)
        assertBool ("октава " <> show octave) (abs (octave - 1) < 0.01)
        assertBool ("шумность " <> show allNoise) (allNoise > 1 && allNoise < 3)
        assertBool ("резкость " <> show allBite) (allBite > 1 && allBite < 3)
    ]

gridTests :: TestTree
gridTests =
  testGroup
    "решётка"
    [ testCase "размер решётки" $ do
        length timbreGrid @?= 13 * 7 * 7
    , testCase "точки различны" $ do
        length (nub timbreGrid) @?= length timbreGrid
    , testCase "яркость растёт по октавам" $ do
        near "Гц" 160 (brightHz (Timbre 2 0 0))
        near "Гц" 320 (brightHz (Timbre 4 0 0))
    , testCase "резкость от медленной атаки к щелчку" $ do
        near "с" 0.2 (attackSec (Timbre 0 0 0))
        assertBool ("щелчок " <> show (attackSec (Timbre 0 0 6))) (attackSec (Timbre 0 0 6) < 0.002)
    , testCase "соседи отличаются одним шагом" $ do
        let t = Timbre 6 3 3
            step a b = abs (timbreBright a - timbreBright b) + abs (timbreNoise a - timbreNoise b) + abs (timbreBite a - timbreBite b)
        sequence_ [step t n @?= 1 | n <- spaceNeighbours timbreSpace t]
        length (spaceNeighbours timbreSpace t) @?= 6
    , testCase "с края решётки соседей меньше" $ do
        length (spaceNeighbours timbreSpace (Timbre 0 0 0)) @?= 3
    ]

embedTests :: TestTree
embedTests =
  testGroup
    "укладка"
    [ testCase "число точек равно числу состояний" $ do
        length (embedIn timbreSpace 2 40 threeStates) @?= 3
    , -- То же свойство, что проверялось у аккордов: близко предсказывающие
      -- состояния обязаны оказаться близко и в новом пространстве.
      testCase "близкие состояния кладутся рядом" $ do
        let ps = embedIn timbreSpace 2 40 threeStates
            d i j = timbreDist (ps !! i) (ps !! j)
        assertBool ("d01=" <> show (d 0 1) <> " d02=" <> show (d 0 2)) (d 0 1 < d 0 2)
        assertBool ("d01=" <> show (d 0 1) <> " d12=" <> show (d 1 2)) (d 0 1 < d 1 2)
    , testCase "укладка взаимно однозначна" $ do
        let ps = embedIn timbreSpace 3 40 fiveStates
        length (nub ps) @?= length ps
    , testCase "искажение конечно" $ do
        let ps = embedIn timbreSpace 3 40 fiveStates
        assertBool "бесконечное искажение" (not (isInfinite (distortionIn timbreSpace ps fiveStates)))
    , testCase "равные расстояния дают равные дуги" $ do
        let ps = embedIn timbreSpace 2 40 equilateral
            ds = [timbreDist (ps !! i) (ps !! j) | i <- [0 .. 2], j <- [i + 1 .. 2]]
        assertBool ("дуги " <> show ds) (maximum ds - minimum ds < 0.5)
    ]
  where
    threeStates =
      [ [0, 0.1, 1.0]
      , [0.1, 0, 1.0]
      , [1.0, 1.0, 0]
      ]
    equilateral =
      [ [0, 1, 1]
      , [1, 0, 1]
      , [1, 1, 0]
      ]
    fiveStates =
      [ [if i == j then 0 else abs (i - j) / 4 | j <- [0 .. 4 :: Double]]
      | i <- [0 .. 4 :: Double]
      ]
