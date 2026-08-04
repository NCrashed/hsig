-- | Композитор: жадный отбор такта и его влияние на модель слушателя.
--
-- Главный тест этапа M5 сравнивает три режима отбора. Он же сторожит
-- найденный дефект: окно сюрприза отбирает кандидатов по правдоподобию под
-- моделью слушателя, смещает статистику потока и делает хуже, чем полное
-- отсутствие выбора. Если кто-то вернёт окно в умолчания, тест упадёт.
module ComposeSpec (tests) where

import Sound.Pred.Compose
import Sound.Pred.Dist
import Sound.Pred.Listener (newListener)
import Sound.Pred.Machine
import Sound.Pred.Orbifold
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Compose"
    [ machineTests
    , errorTests
    , selectionTests
    ]

-- | Кольцо из четырёх состояний с тремя символами. Выходы намеренно
-- острые и разные по состояниям: иначе состояния бисимилярны и учить
-- нечему.
ring :: Machine Int Int
ring =
  Machine
    { machineStart = 0
    , machineStates = [0 .. 3]
    , machineOut = dist . weights
    , machineStep = \s x -> (s + step x) `mod` 4
    }
  where
    step x = case x of
      0 -> 0
      1 -> 1
      _ -> 3
    weights s
      | even s = [(0, 0.80), (1, 0.14), (2, 0.06)]
      | otherwise = [(0, 0.10), (1, 0.78), (2, 0.12)]

tonality :: Scale
tonality = mkScale "minor"

chordOf :: Int -> Chord
chordOf s = mkChord [s, s + 2, s + 4]

opts :: BarOpts
opts = defaultBarOpts {barLen = 6, barCands = 12, barProbes = 32, barOrder = 3, barVlMax = 12}

runBars :: BarOpts -> Int -> [Bar Int Int]
runBars o n = compose o ring chordOf tonality [0, 1, 2] n

-- | Ошибка модели перед последним тактом: чем меньше, тем лучше слушатель
-- знает истинный процесс.
finalError :: [Bar Int Int] -> Double
finalError bs = case reverse bs of
  (b : _) -> barError b
  [] -> error "пустая пьеса"

machineTests :: TestTree
machineTests =
  testGroup
    "прогон машины"
    [ testCase "след детерминирован по seed" $ do
        take 20 (map snd (runMachine ring 5)) @?= take 20 (map snd (runMachine ring 5))
    , testCase "разные seed дают разные следы" $ do
        assertBool
          "следы совпали"
          (take 40 (map snd (runMachine ring 5)) /= take 40 (map snd (runMachine ring 6)))
    , testCase "состояния идут по переходу машины" $ do
        let path = take 30 (runMachine ring 7)
            steps = [machineStep ring s x | (s, x) <- path]
        map fst (drop 1 path) @?= init steps
    , testCase "пробы дают историю нужной длины и состояние после неё" $ do
        let ps = probesOf ring 3 10 11
        length ps @?= 10
        sequence_ [length h @?= 3 | (h, _) <- ps]
        sequence_ [assertBool "состояние вне машины" (s `elem` machineStates ring) | (_, s) <- ps]
    ]

errorTests :: TestTree
errorTests =
  testGroup
    "ошибка модели"
    [ testCase "пустой набор проб даёт ноль" $ do
        modelError ring (newListener 3 [0, 1, 2]) [] @?= 0
    , testCase "необученный слушатель ошибается заметно" $ do
        let e = modelError ring (newListener 3 [0, 1, 2]) (probesOf ring 3 32 13)
        assertBool ("ошибка " <> show e) (e > 0.3)
    , testCase "ошибка неотрицательна" $ do
        sequence_
          [ assertBool ("отрицательная ошибка в такте: " <> show (barError b)) (barError b >= 0)
          | b <- runBars opts 6
          ]
    ]

selectionTests :: TestTree
selectionTests =
  testGroup
    "режимы отбора"
    [ testCase "такты имеют заданную длину" $ do
        sequence_
          [ do
            length (barSyms b) @?= barLen opts
            length (barStates b) @?= barLen opts
          | b <- runBars opts 6
          ]
    , -- Ради этого числа всё и затевалось: педагогическая выборка обязана
      -- обгонять честную, иначе жадный поиск не нужен.
      testCase "жадный отбор учит лучше, чем отсутствие выбора" $ do
        let greedy = finalError (runBars opts 10)
            control = finalError (runBars opts {barCands = 1} 10)
        assertBool
          ("жадный " <> show greedy <> ", контроль " <> show control)
          (greedy < control)
    , -- Сторож дефекта. Окно сюрприза отбирает по правдоподобию под
      -- слушателем и потому смещает поток: результат хуже, чем вообще без
      -- выбора. Вернуть окно в умолчания нельзя.
      testCase "окно сюрприза делает хуже, чем отсутствие выбора" $ do
        let windowed = finalError (runBars opts {barWindow = Just (0.25, 0.8)} 10)
            control = finalError (runBars opts {barCands = 1} 10)
        assertBool
          ("с окном " <> show windowed <> ", контроль " <> show control)
          (windowed > control)
    , testCase "умолчание идёт без окна" $ do
        assertBool "окно вернулось в умолчания" (barWindow defaultBarOpts == Nothing)
    ]
