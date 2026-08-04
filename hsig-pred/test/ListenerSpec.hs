-- | Слушатель: сходимость сюрприза к энтропийной скорости.
--
-- Приёмочный тест этапа M4 (docs/PRED.md, разд. 5 и 8): на golden mean
-- слушатель первого порядка выходит ровно на h_mu, на even застревает выше
-- на вычислимую величину, и избыток падает с ростом порядка.
module ListenerSpec (tests) where

import Sound.Pred.Dist
import Sound.Pred.Kernel
import Sound.Pred.Listener
import Sound.Pred.Machine
import Sound.Pred.Model
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Listener"
    [ basicTests
    , convergenceTests
    ]

-- | Длина потока для оценок. Стандартная ошибка среднего сюрприза при
-- такой длине около 0.01 бита, отсюда допуски ниже.
streamLen :: Int
streamLen = 40000

-- | Истинная энтропийная скорость обеих машин при p = 1/2.
hTrue :: Double
hTrue = 2 / 3

-- | Оценка слушателя первого порядка на even: (1/3)*H(1/2) + (2/3)*H(3/4).
hEvenOrder1 :: Double
hEvenOrder1 = (1 / 3) * 1 + (2 / 3) * hBits (3 / 4)
  where
    hBits q = negate (q * logBase 2 q + (1 - q) * logBase 2 (1 - q))

streamOf :: Machine TwoState Int -> Int -> [Int]
streamOf m seed = take streamLen (generateSeeded seed (toPred m))

-- | Средний сюрприз по второй половине потока: без переходного участка.
rateOf :: Int -> [Int] -> Double
rateOf k xs = tailMean 0.5 (onlineSurprisals (newListener k [0, 1]) xs)

basicTests :: TestTree
basicTests =
  testGroup
    "основы"
    [ testCase "пустой слушатель предсказывает равномерно" $ do
        distPairs (predictNext (newListener 2 [0 :: Int, 1])) @?= [(0, 0.5), (1, 0.5)]
    , testCase "неслыханный символ имеет конечный сюрприз" $ do
        let l = trainOn (newListener 2 [0 :: Int, 1]) (replicate 100 0)
        assertBool "бесконечный сюрприз" (not (isInfinite (surprisalOf (predictNext l) 1)))
    , testCase "повторение снижает сюрприз" $ do
        case onlineSurprisals (newListener 2 [0 :: Int, 1]) (replicate 50 0) of
          [] -> assertFailure "пустой список сюрпризов"
          (s0 : rest) ->
            assertBool ("первый " <> show s0 <> ", последний " <> show (last rest)) (last rest < s0)
    , testCase "история ограничена порядком" $ do
        length (listenerHist (trainOn (newListener 3 [0 :: Int, 1]) (replicate 20 0))) @?= 3
    , testCase "предсказание нормировано" $ do
        let l = trainOn (newListener 4 [0 :: Int, 1]) (take 500 (generateSeeded 2 (toPred (goldenMean 0.5))))
        assertBool "не единица" (abs (sum (map snd (distPairs (predictNext l))) - 1) < 1e-9)
    ]

convergenceTests :: TestTree
convergenceTests =
  testGroup
    "сходимость"
    [ -- Golden mean это цепь порядка один, значит слушатель порядка один
      -- обязан выйти на истинную энтропийную скорость, а не приблизиться.
      testCase "golden mean: порядок 1 выходит на h_mu = 2/3" $ do
        let r = rateOf 1 (streamOf (goldenMean 0.5) 101)
        assertBool ("оценка " <> show r <> ", истина " <> show hTrue) (abs (r - hTrue) < 0.02)
    , testCase "golden mean: больший порядок не портит" $ do
        let r = rateOf 6 (streamOf (goldenMean 0.5) 102)
        assertBool ("оценка " <> show r) (abs (r - hTrue) < 0.03)
    , -- У even марковского порядка нет, и слушатель порядка один даёт
      -- вычислимую наперёд оценку, отличную от истины.
      testCase "even: порядок 1 даёт 0.874 вместо 2/3" $ do
        let r = rateOf 1 (streamOf (evenProcess 0.5) 103)
        assertBool ("оценка " <> show r <> ", предсказано " <> show hEvenOrder1) (abs (r - hEvenOrder1) < 0.03)
    , testCase "even: избыток над h_mu строго положителен при порядке 1" $ do
        let r = rateOf 1 (streamOf (evenProcess 0.5) 104)
        assertBool ("избыток " <> show (r - hTrue)) (r - hTrue > 0.15)
    , -- Пробеги единиц длиннее порядка редки, поэтому избыток падает, а не
      -- держится. Проверяем именно падение, а не мифическую несходимость.
      testCase "even: избыток падает с ростом порядка" $ do
        let xs = streamOf (evenProcess 0.5) 105
            r1 = rateOf 1 xs
            r4 = rateOf 4 xs
            r8 = rateOf 8 xs
        assertBool ("r1=" <> show r1 <> " r4=" <> show r4) (r4 < r1 - 0.1)
        assertBool ("r4=" <> show r4 <> " r8=" <> show r8) (r8 <= r4 + 0.02)
        assertBool ("r8=" <> show r8 <> ", h_mu=" <> show hTrue) (abs (r8 - hTrue) < 0.05)
    , -- Разделяющий тест: при одинаковых h_mu и C_mu процессы для
      -- слушателя конечного порядка совершенно разные.
      testCase "процессы с одними инвариантами различаются на слух" $ do
        let g = rateOf 1 (streamOf (goldenMean 0.5) 106)
            e = rateOf 1 (streamOf (evenProcess 0.5) 107)
        assertBool ("golden " <> show g <> ", even " <> show e) (e - g > 0.15)
    ]
