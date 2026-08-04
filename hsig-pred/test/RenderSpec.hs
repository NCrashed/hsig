-- | Рендер: регистр, слияние повторов, длительность из информации.
module RenderSpec (tests) where

import Sound.Pred.Orbifold
import Sound.Pred.Render
import Sound.Sig.Score (Arc (..), Event (..), Pattern, queryArc)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Render"
    [ runTests
    , lineTests
    , patternTests
    ]

major :: Scale
major = mkScale "major"

near :: String -> Double -> Double -> Assertion
near what expected actual =
  assertBool
    (what <> ": ожидалось " <> show expected <> ", получено " <> show actual)
    (abs (expected - actual) < 1e-9)

runTests :: TestTree
runTests =
  testGroup
    "слияние повторов"
    [ testCase "пустой вход" $ do
        runsOf ([] :: [Int]) @?= []
    , testCase "все разные" $ do
        runsOf [1 :: Int, 2, 3] @?= [(1, 1), (1, 2), (1, 3)]
    , testCase "все одинаковые" $ do
        runsOf [7 :: Int, 7, 7, 7] @?= [(4, 7)]
    , testCase "смешанный случай" $ do
        runsOf [1 :: Int, 1, 2, 3, 3, 3] @?= [(2, 1), (1, 2), (3, 3)]
    , testCase "сумма длин сохраняется" $ do
        let xs = [0 :: Int, 0, 1, 1, 1, 2, 0, 0]
        sum (map fst (runsOf xs)) @?= length xs
    , testCase "chunksOf режет ровно" $ do
        chunksOf 3 [1 :: Int .. 7] @?= [[1, 2, 3], [4, 5, 6], [7]]
    ]

lineTests :: TestTree
lineTests =
  testGroup
    "регистр"
    [ testCase "голоса первого аккорда идут снизу вверх" $ do
        let [vs] = voiceLines major 36 [mkChord [0, 2, 4]]
        assertBool ("голоса " <> show vs) (and (zipWith (<) vs (drop 1 vs)))
    , testCase "первый голос не ниже базы" $ do
        let [vs] = voiceLines major 36 [mkChord [0, 2, 4]]
        assertBool ("голоса " <> show vs) (minimum vs >= 36)
    , -- Смысл выбора ближайшей октавы: голос не прыгает через регистр,
      -- когда ступень переваливает за октаву лада.
      testCase "голоса не скачут больше чем на полутон по кругу" $ do
        let cs = [mkChord [i, i + 2, i + 4] | i <- [0 .. 7]]
            ls = voiceLines major 36 cs
            jumps = concat (zipWith (\a b -> zipWith (\x y -> abs (x - y)) a b) ls (drop 1 ls))
        assertBool ("скачки " <> show jumps) (maximum jumps <= 6)
    , testCase "линия из одной ступени повторяет себя" $ do
        degreeLine major 48 [2, 2, 2] @?= replicate 3 (head (degreeLine major 48 [2]))
    , testCase "герцы удваиваются на октаву" $ do
        near "hz" (2 * hzOf 55 0) (hzOf 55 12)
    ]

-- | Сколько событий с атакой в первом цикле паттерна.
onsetsIn :: Pattern a -> Int
onsetsIn p = length [e | e <- queryArc p (Arc 0 1), fmap arcStart (eventWhole e) == Just (arcStart (eventPart e))]

patternTests :: TestTree
patternTests =
  testGroup
    "длительность из информации"
    [ -- Восемь одинаковых событий это одна долгая нота, а не восемь атак:
      -- предсказуемое событие не несёт информации и не заслуживает своей
      -- атаки.
      testCase "повтор сливается в одну ноту" $ do
        onsetsIn (melodyPattern 55 8 (replicate 8 12)) @?= 1
    , testCase "все разные события дают все атаки" $ do
        onsetsIn (melodyPattern 55 8 [0, 1, 2, 3, 4, 5, 6, 7]) @?= 8
    , testCase "смешанный такт даёт по атаке на пробег" $ do
        onsetsIn (melodyPattern 55 8 [0, 0, 0, 3, 3, 7, 7, 7]) @?= 3
    , testCase "слияние не переходит границу такта" $ do
        -- Два такта из одинаковых событий это две ноты, а не одна:
        -- метрическая сетка задана снаружи и не выводится из материала.
        let p = melodyPattern 55 4 (replicate 8 12)
        onsetsIn p @?= 1
        length [e | e <- queryArc p (Arc 0 2), fmap arcStart (eventWhole e) == Just (arcStart (eventPart e))] @?= 2
    , testCase "гармония даёт по атаке на голос" $ do
        onsetsIn (harmonyPattern 27.5 4 (replicate 4 [0, 4, 7])) @?= 3
    ]
