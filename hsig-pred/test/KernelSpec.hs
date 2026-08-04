-- | Крошечные ядра: подстановки и двухсостоятельные процессы.
module KernelSpec (tests) where

import Data.Bits (popCount)
import Data.List (group, isInfixOf)
import Sound.Pred.Dist
import Sound.Pred.Kernel
import Sound.Pred.Machine
import Sound.Pred.Model
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Kernel"
    [ substTests
    , thueMorseTests
    , processTests
    ]

substTests :: TestTree
substTests =
  testGroup
    "подстановки"
    [ -- Префиксы из таблицы docs/PRED.md, разд. 5.
      testCase "Туэ-Морс" $ do
        take 8 thueMorseWord @?= "abbabaab"
    , testCase "удвоение периода" $ do
        take 8 periodDoublingWord @?= "abaaabab"
    , testCase "Фибоначчи" $ do
        take 8 fibonacciWord @?= "abaababa"
    , testCase "слово это неподвижная точка морфизма" $ do
        let n = 200
            image = concatMap (\c -> maybe [c] id (lookup c thueMorse)) (take n thueMorseWord)
        take n image @?= take n thueMorseWord
    , testCase "длины слов Фибоначчи растут по Фибоначчи" $ do
        -- |sigma^n(a)| это числа Фибоначчи: проверка независимая от знака.
        let iterN k = iterate (concatMap (\c -> maybe [c] id (lookup c fibonacci))) "a" !! k
        map (length . iterN) [0 .. 7] @?= [1, 2, 3, 5, 8, 13, 21, 34]
    , testCase "модель подстановки детерминирована" $ do
        take 8 (generateSeeded 1 (substPred thueMorse 'a')) @?= "abbabaab"
    , testCase "подстановка никогда не удивляет саму себя" $ do
        sum (surprisals (substPred thueMorse 'a') (take 500 thueMorseWord)) @?= 0
    , testCase "энтропия предсказания подстановки нулевая" $ do
        sum (entropies (substPred fibonacci 'a') (take 200 fibonacciWord)) @?= 0
    ]

thueMorseTests :: TestTree
thueMorseTests =
  testGroup
    "Туэ-Морс независимо"
    [ -- Тот же ряд, посчитанный совсем иначе: чётность числа единиц в
      -- двоичной записи индекса. Совпадение на 256 членах исключает
      -- ошибку в ленивом узле подстановки.
      testCase "совпадает с чётностью popcount" $ do
        let byBits = [if even (popCount (i :: Int)) then 'a' else 'b' | i <- [0 .. 255]]
        take 256 thueMorseWord @?= byBits
    , testCase "нет куба, то есть трёх одинаковых блоков подряд" $ do
        let w = take 400 thueMorseWord
            cubes =
              [ (i, k)
              | k <- [1 .. 6]
              , i <- [0 .. length w - 3 * k - 1]
              , let b = take k (drop i w)
              , take (3 * k) (drop i w) == concat (replicate 3 b)
              ]
        cubes @?= []
    ]

processTests :: TestTree
processTests =
  testGroup
    "двухсостоятельные процессы"
    [ -- Определяющее свойство even: единицы идут блоками чётной длины.
      -- Именно оно требует бесконечного марковского порядка при двух
      -- причинных состояниях.
      testCase "even даёт блоки единиц чётной длины" $ do
        let xs = take 5000 (generateSeeded 21 (toPred (evenProcess 0.5)))
            runs = [length g | g <- group xs, head g == 1]
            complete = if null runs then [] else init runs
        assertBool ("нечётные блоки: " <> show (take 5 (filter odd complete))) (all even complete)
    , testCase "golden mean не даёт двух нулей подряд" $ do
        let xs = take 5000 (generateSeeded 22 (toPred (goldenMean 0.5)))
        assertBool "нашлось 00" (not ([0, 0] `isInfixOf` xs))
    , testCase "even при p=1/2 даёт примерно два нуля на три символа" $ do
        -- Доля нулей это pi(A) * p = (2/3)(1/2) = 1/3.
        let n = 20000
            xs = take n (generateSeeded 23 (toPred (evenProcess 0.5)))
            share = fromIntegral (length (filter (== 0) xs)) / fromIntegral n :: Double
        assertBool ("доля нулей " <> show share) (abs (share - 1 / 3) < 0.02)
    , testCase "выход машины совпадает с предсказанием модели" $ do
        let m = evenProcess 0.6
        distPairs (predict (toPred m)) @?= distPairs (machineOut m (machineStart m))
    ]
