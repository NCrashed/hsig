-- | Пространство аккордов: метрика голосоведения и укладка состояний.
module OrbifoldSpec (tests) where

import Data.List (sort)
import Sound.Pred.Orbifold
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Orbifold"
    [ degreeTests
    , transposeTests
    , metricTests
    , embedTests
    ]

major :: Scale
major = mkScale "major"

near :: String -> Double -> Double -> Assertion
near what expected actual =
  assertBool
    (what <> ": ожидалось " <> show expected <> ", получено " <> show actual)
    (abs (expected - actual) < 1e-9)

-- Трезвучия мажорного лада по ступеням.
tonic, superTonic, mediant, subDominant, submediant :: Chord
tonic = mkChord [0, 2, 4]
superTonic = mkChord [1, 3, 5]
mediant = mkChord [2, 4, 6]
subDominant = mkChord [3, 5, 7]
submediant = mkChord [5, 7, 9]

degreeTests :: TestTree
degreeTests =
  testGroup
    "ступени в полутоны"
    [ testCase "тоника это мажорное трезвучие" $ do
        chordSemis major tonic @?= [0, 4, 7]
    , testCase "вторая ступень даёт минорное" $ do
        chordSemis major superTonic @?= [2, 5, 9]
    , testCase "третья ступень даёт минорное" $ do
        chordSemis major mediant @?= [4, 7, 11]
    , testCase "четвёртая ступень перешагивает октаву" $ do
        chordSemis major subDominant @?= [5, 9, 12]
    , testCase "шестая ступень даёт минорное через октаву" $ do
        chordSemis major submediant @?= [9, 12, 16]
    ]

transposeTests :: TestTree
transposeTests =
  testGroup
    "диатонический перенос"
    [ -- Стержень раздела: сдвиг на одну ступень меняет качество аккорда.
      -- Реальный ответ сохранил бы интервалы и ушёл из лада, тональный
      -- остаётся в ладу и искажает. Это разные операции, и здесь
      -- реализована вторая.
      testCase "перенос на ступень не сохраняет интервалы" $ do
        let gaps c = let s = sort (chordSemis major c) in zipWith (-) (drop 1 s) s
        gaps tonic @?= [4, 3]
        gaps (transposeDeg 1 tonic) @?= [3, 4]
        assertBool "интервалы совпали" (gaps tonic /= gaps (transposeDeg 1 tonic))
    , testCase "перенос на ступень остаётся в ладу" $ do
        let inScale c = all (`elem` map (`mod` 12) (scaleSteps major)) (map (`mod` 12) (chordSemis major c))
        sequence_ [assertBool ("вышли из лада на " <> show k) (inScale (transposeDeg k tonic)) | k <- [0 .. 13]]
    , testCase "перенос на размер лада это та же гармония" $ do
        near "vl" 0 (vlDist major (transposeDeg 7 tonic) tonic)
    , testCase "перенос это действие группы" $ do
        transposeDeg 3 (transposeDeg 4 tonic) @?= transposeDeg 7 tonic
    ]

metricTests :: TestTree
metricTests =
  testGroup
    "расстояние голосоведения"
    [ -- Три парсимонных хода неоримановой теории с известными размерами.
      testCase "тоника к медианте это один полутон" $ do
        near "vl" 1 (vlDist major tonic mediant)
    , testCase "тоника к субмедианте это два полутона" $ do
        near "vl" 2 (vlDist major tonic submediant)
    , testCase "тоника к субдоминанте это три полутона" $ do
        near "vl" 3 (vlDist major tonic subDominant)
    , testCase "тоника ко второй ступени это пять полутонов" $ do
        near "vl" 5 (vlDist major tonic superTonic)
    , testCase "расстояние до себя нулевое" $ do
        sequence_ [near "vl" 0 (vlDist major c c) | c <- allChords]
    , testCase "симметрия" $ do
        sequence_
          [near "vl" (vlDist major a b) (vlDist major b a) | a <- allChords, b <- allChords]
    , testCase "неравенство треугольника" $ do
        sequence_
          [ assertBool ("треугольник нарушен: " <> show (a, b, c)) $
            vlDist major a c <= vlDist major a b + vlDist major b c + 1e-9
          | a <- allChords
          , b <- allChords
          , c <- allChords
          ]
    , testCase "порядок голосов не важен" $ do
        near "vl" 0 (vlDist major (Chord [0, 2, 4]) (Chord [4, 0, 2]))
    ]
  where
    allChords = [mkChord [i, i + 2, i + 4] | i <- [0 .. 6]]

embedTests :: TestTree
embedTests =
  testGroup
    "укладка"
    [ testCase "число аккордов равно числу состояний" $ do
        length (embed defaultEmbed threeStates) @?= 3
    , testCase "у всех аккордов заданная голосность" $ do
        sequence_
          [ length (chordDegrees c) @?= embedVoices defaultEmbed
          | c <- embed defaultEmbed threeStates
          ]
    , testCase "укладка не хуже стартовой" $ do
        let cs = embed defaultEmbed threeStates
            naive = [mkChord [i, i + 2, i + 4] | i <- [0 .. 2]]
        assertBool
          ("стресс " <> show (stress defaultEmbed threeStates cs))
          (stress defaultEmbed threeStates cs <= stress defaultEmbed threeStates naive + 1e-9)
    , -- Главное свойство: близко предсказывающие состояния обязаны
      -- оказаться близко по голосоведению. Проверяется порядок, а не
      -- абсолютные числа: конкретные значения зависят от лада.
      testCase "близкие состояния кладутся рядом" $ do
        let cs = embed defaultEmbed threeStates
            d01 = vlDist major (cs !! 0) (cs !! 1)
            d02 = vlDist major (cs !! 0) (cs !! 2)
            d12 = vlDist major (cs !! 1) (cs !! 2)
        assertBool ("d01=" <> show d01 <> " d02=" <> show d02) (d01 < d02)
        assertBool ("d01=" <> show d01 <> " d12=" <> show d12) (d01 < d12)
    , testCase "равные расстояния дают равные дуги" $ do
        let cs = embed defaultEmbed equilateral
            ds = [vlDist major (cs !! i) (cs !! j) | i <- [0 .. 2], j <- [i + 1 .. 2]]
        assertBool ("дуги " <> show ds) (maximum ds - minimum ds <= 1)
    , testCase "липшицева константа конечна и положительна" $ do
        let cs = embed defaultEmbed threeStates
            l = lipschitz major cs threeStates
        assertBool ("L = " <> show l) (l > 0 && not (isInfinite l))
    , testCase "искажение не меньше единицы" $ do
        let cs = embed defaultEmbed threeStates
        assertBool "искажение меньше единицы" (distortion major cs threeStates >= 1 - 1e-9)
    ]
  where
    -- Два состояния предсказывают почти одинаково, третье далеко от обоих.
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
