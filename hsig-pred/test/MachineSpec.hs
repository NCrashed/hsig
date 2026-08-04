-- | Явная эпсилон-машина и её инварианты.
--
-- Тесты сверяются с замкнутыми формами из docs/PRED.md, разд. 5, а не с
-- выводом предыдущего запуска: эталонный вывод фиксирует баг так же
-- охотно, как правильное поведение.
module MachineSpec (tests) where

import Sound.Pred.Dist
import Sound.Pred.Kernel
import Sound.Pred.Machine
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Machine"
    [ stationaryTests
    , invariantTests
    ]

near :: String -> Double -> Double -> Assertion
near what expected actual =
  assertBool
    (what <> ": ожидалось " <> show expected <> ", получено " <> show actual)
    (abs (expected - actual) < 1e-9)

-- Замкнутые формы для обеих двухсостоятельных машин ---------------------------

hBits :: Double -> Double
hBits q = negate (q * logBase 2 q + (1 - q) * logBase 2 (1 - q))

-- | Стационарный вес первого состояния.
piA :: Double -> Double
piA p = 1 / (2 - p)

-- | Энтропийная скорость: неопределённость есть только в первом состоянии.
hClosed :: Double -> Double
hClosed p = hBits p / (2 - p)

-- | Статистическая сложность: энтропия стационарного распределения.
cClosed :: Double -> Double
cClosed p = hBits (piA p)

stationaryTests :: TestTree
stationaryTests =
  testGroup
    "стационарное распределение"
    [ testCase "even при p=1/2 это (2/3, 1/3)" $ do
        near "pi(A)" (2 / 3) (probOf SA (stationary (evenProcess 0.5)))
        near "pi(B)" (1 / 3) (probOf SB (stationary (evenProcess 0.5)))
    , testCase "golden mean при p=1/2 это (2/3, 1/3)" $ do
        near "pi(A)" (2 / 3) (probOf SA (stationary (goldenMean 0.5)))
    , testCase "замкнутая форма pi(A) = 1/(2-p)" $ do
        sequence_
          [ near ("p=" <> show p) (piA p) (probOf SA (stationary (evenProcess p)))
          | p <- [0.1, 0.3, 0.5, 0.7, 0.9]
          ]
    , testCase "стационарное нормировано" $ do
        near "сумма" 1 (sum (map snd (distPairs (stationary (evenProcess 0.7)))))
    ]

invariantTests :: TestTree
invariantTests =
  testGroup
    "инварианты"
    [ testCase "h_mu при p=1/2 это 2/3 бита" $ do
        near "h_mu" (2 / 3) (entropyRate (evenProcess 0.5))
    , testCase "C_mu при p=1/2 это H(2/3)" $ do
        near "C_mu" 0.9182958340544896 (statComplexity (evenProcess 0.5))
    , testCase "h_mu совпадает с замкнутой формой" $ do
        sequence_
          [ near ("p=" <> show p) (hClosed p) (entropyRate (evenProcess p))
          | p <- [0.1, 0.3, 0.5, 0.7, 0.9]
          ]
    , testCase "C_mu совпадает с замкнутой формой" $ do
        sequence_
          [ near ("p=" <> show p) (cClosed p) (statComplexity (evenProcess p))
          | p <- [0.1, 0.3, 0.5, 0.7, 0.9]
          ]
    , -- Машины изоморфны, процессы разные: одинаковые числа тут не
      -- совпадение, а проверка того, что инварианты считают топологию, а
      -- не разметку выходов.
      testCase "even и golden mean имеют одни инварианты" $ do
        near "h_mu" (entropyRate (evenProcess 0.7)) (entropyRate (goldenMean 0.7))
        near "C_mu" (statComplexity (evenProcess 0.7)) (statComplexity (goldenMean 0.7))
    , testCase "детерминированная машина имеет нулевую энтропийную скорость" $ do
        near "h_mu" 0 (entropyRate (evenProcess 1e-12))
    ]
