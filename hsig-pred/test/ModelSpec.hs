-- | Коалгебра предсказания: генерация, схлопывание смеси, иерархия.
module ModelSpec (tests) where

import Sound.Pred.Dist
import Sound.Pred.Model
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Model"
    [ walkTests
    , generateTests
    , infoTests
    , mixtureTests
    , parTests
    , nestTests
    ]

near :: String -> Double -> Double -> Assertion
near what expected actual =
  assertBool
    (what <> ": ожидалось " <> show expected <> ", получено " <> show actual)
    (abs (expected - actual) < 1e-9)

-- | Считалка по модулю: предсказывает остаток и всегда права.
counter :: Int -> Pred Int
counter m = unfoldPred (\s -> dirac (s `mod` m)) (\s _ -> s + 1) 0

-- | Независимые броски с перекосом на 'a'.
biased :: Double -> Pred Char
biased p = constPred (dist [('a', p), ('b', 1 - p)])

walkTests :: TestTree
walkTests =
  testGroup
    "прогулка"
    [ testCase "детерминированная модель предсказывает точно" $ do
        map (support . predict) (take 4 (walk (counter 3) [0, 1, 2])) @?= [[0], [1], [2], [0]]
    , testCase "состояний на одно больше наблюдений" $ do
        length (walk (counter 3) [0, 1, 2]) @?= 4
    , testCase "наблюдение вне носителя не роняет модель" $ do
        -- Считалка игнорирует значение наблюдения, важен сам факт шага.
        support (predict (observe (counter 3) 7)) @?= [1]
    ]

generateTests :: TestTree
generateTests =
  testGroup
    "генерация"
    [ testCase "детерминированная модель даёт свой след" $ do
        take 7 (generateSeeded 1 (counter 3)) @?= [0, 1, 2, 0, 1, 2, 0]
    , testCase "длина равна числу поданных чисел" $ do
        length (generate (biased 0.5) (replicate 12 0.3)) @?= 12
    , testCase "частоты сходятся к весам" $ do
        let n = 20000 :: Int
            xs = take n (generateSeeded 7 (biased 0.75))
            freq = fromIntegral (length (filter (== 'a') xs)) / fromIntegral n :: Double
        assertBool ("частота 'a' = " <> show freq) (abs (freq - 0.75) < 0.01)
    ]

infoTests :: TestTree
infoTests =
  testGroup
    "информация"
    [ testCase "точная модель не удивляется" $ do
        surprisals (counter 3) [0, 1, 2, 0] @?= [0, 0, 0, 0]
    , testCase "неожиданность это минус логарифм веса" $ do
        surprisals (constPred (uniform "abcd")) "a" @?= [2]
    , testCase "правдоподобие складывается из неожиданностей" $ do
        near "L" (-4) (logLik (constPred (uniform "abcd")) "ab")
    , testCase "энтропия детерминированной модели нулевая" $ do
        entropies (counter 3) [0, 1] @?= [0, 0]
    ]

mixtureTests :: TestTree
mixtureTests =
  testGroup
    "смесь"
    [ testCase "одна компонента это она сама" $ do
        near "p(a)" 0.7 (probOf 'a' (predict (mixture [(1, biased 0.7)])))
        near "p(b)" 0.3 (probOf 'b' (predict (mixture [(1, biased 0.7)])))
    , testCase "до наблюдений это взвешенное среднее" $ do
        near "p" 0.5 (probOf 'a' (predict (mixture [(1, biased 0.9), (1, biased 0.1)])))
    , -- Апостериор после k наблюдений 'a' равен 0.9^k / (0.9^k + 0.1^k),
      -- отсюда предсказание 0.1 + 0.8 * w. Число замкнутое, не эталонное.
      testCase "наблюдения схлопывают смесь к породившей" $ do
        let m = mixture [(1, biased 0.9), (1, biased 0.1)]
            heard = foldl observe m "aaaa"
            w = 0.9 ** 4 / (0.9 ** 4 + 0.1 ** 4)
        near "p" (0.1 + 0.8 * w) (probOf 'a' (predict heard))
    , testCase "невозможная компонента умирает навсегда" $ do
        let dead = constPred (dirac 'b')
            m = mixture [(1, dead), (1, biased 0.5)]
            heard = observe m 'a'
        near "p" 0.5 (probOf 'a' (predict heard))
    , testCase "смесь не удивляется сильнее лучшей компоненты" $ do
        let m = mixture [(1, biased 0.9), (1, biased 0.1)]
            best = biased 0.9
        assertBool
          "смесь хуже компоненты"
          (logLik m (replicate 8 'a') > logLik best (replicate 8 'a') - 1.01)
    ]

parTests :: TestTree
parTests =
  testGroup
    "произведение"
    [ testCase "совместное распределение факторизуется" $ do
        let p = par (biased 0.75) (constPred (uniform "xy"))
        near "p" 0.375 (probOf ('a', 'x') (predict p))
    , testCase "голоса идут независимо" $ do
        let p = par (counter 2) (counter 3)
        take 4 (generateSeeded 3 p) @?= [(0, 0), (1, 1), (0, 2), (1, 0)]
    ]

nestTests :: TestTree
nestTests =
  testGroup
    "иерархия"
    [ testCase "детерминированный верх даёт блоки" $ do
        let upper = unfoldPred (\s -> dirac (s `mod` 2)) (\s _ -> s + 1) (0 :: Int)
            leaf i = constPred (dirac (if i == 0 then 'a' else 'b'))
        take 8 (generateSeeded 5 (nest 2 upper leaf)) @?= "aabbaabb"
    , -- Верхний символ латентный: до первой ноты блок неизвестен, после
      -- первой ноты он определён, на границе блока неопределённость
      -- возвращается. Это и есть схлопывание суперпозиции.
      testCase "латентный верх выводится из первой ноты блока" $ do
        let upper = constPred (uniform [0 :: Int, 1])
            leaf i = constPred (dirac (if i == 0 then 'a' else 'b'))
            m0 = nest 2 upper leaf
            m1 = observe m0 'a'
            m2 = observe m1 'a'
        near "старт" 0.5 (probOf 'a' (predict m0))
        near "внутри блока" 1.0 (probOf 'a' (predict m1))
        near "граница блока" 0.5 (probOf 'a' (predict m2))
    , testCase "длинная генерация не разносит ветвление" $ do
        let upper = constPred (uniform [0 :: Int, 1])
            leaf i = constPred (dist [('a', if i == 0 then 0.8 else 0.2), ('b', if i == 0 then 0.2 else 0.8)])
            xs = take 2000 (generateSeeded 11 (nest 4 upper leaf))
        length xs @?= 2000
    ]
