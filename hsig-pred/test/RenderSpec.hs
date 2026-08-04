-- | Рендер: регистр, слияние повторов, длительность из информации.
module RenderSpec (tests) where

import Sound.Pred.Orbifold
import Sound.Pred.Render
import Sound.Sig.Score (Arc (..), Event (..), Note (..), Pattern, queryArc)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Render"
    [ runTests
    , lineTests
    , patternTests
    , accentTests
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
        let vs = concat (voiceLines major 36 [mkChord [0, 2, 4]])
        assertBool ("голоса " <> show vs) (and (zipWith (<) vs (drop 1 vs)))
    , testCase "первый голос не ниже базы" $ do
        let vs = concat (voiceLines major 36 [mkChord [0, 2, 4]])
        assertBool ("голоса " <> show vs) (minimum vs >= 36)
    , -- Размен, на который пришлось пойти. Без окна каждый шаг был не
      -- больше полутона по кругу, зато голос уползал без предела. С окном
      -- шаги остаются малыми почти всегда, но на краю окна случается
      -- октавный возврат. Скачок на октаву слышен как смена регистра и это
      -- приемлемо; уход на три октавы неприемлем.
      testCase "шаги малые, возвраты не больше октавы" $ do
        let cs = [mkChord [i, i + 2, i + 4] | k <- [0 .. 49 :: Int], i <- [0, 3, 5, 1, 6, 2, 4, k `mod` 7]]
            ls = voiceLines major 36 cs
            jumps = concat (zipWith (zipWith (\x y -> abs (x - y))) ls (drop 1 ls))
            small = length (filter (<= 6) jumps)
            share = fromIntegral small / fromIntegral (length jumps) :: Double
        assertBool ("наибольший скачок " <> show (maximum jumps)) (maximum jumps <= 12)
        assertBool ("доля малых шагов " <> show share) (share > 0.85)
    , testCase "линия из одной ступени повторяет себя" $ do
        degreeLine major 48 [2, 2, 2] @?= concat (replicate 3 (degreeLine major 48 [2]))
    , testCase "герцы удваиваются на октаву" $ do
        near "hz" (2 * hzOf 55 0) (hzOf 55 12)
    , -- Сторож дефекта, который слышно как медленное уползание вверх.
      -- Выбор ближайшей октавы без окна это случайное блуждание: шаг мал,
      -- но ограничения нет, и за несколько сотен аккордов голос уходит на
      -- октавы. Именно на этой длине дефект и проявлялся.
      testCase "голоса не уползают на длинной последовательности" $ do
        let cs = [mkChord [i, i + 2, i + 4] | k <- [0 .. 99 :: Int], i <- [0, 3, 5, 1, 6, 2, 4, k `mod` 7]]
            ls = voiceLines major 36 cs
            allVals = concat ls
        assertBool
          ("диапазон " <> show (minimum allVals, maximum allVals))
          (maximum allVals - minimum allVals < 40)
    , testCase "окно соблюдается явно" $ do
        let cs = [mkChord [i, i + 2, i + 4] | i <- concat (replicate 50 [0, 4, 1, 5, 2, 6, 3])]
            ls = voiceLinesIn major 36 5 cs
            starts = case ls of
              (v : _) -> v
              [] -> []
            offs = concat [zipWith (\h x -> abs (x - h)) starts v | v <- ls]
        assertBool ("максимальный уход " <> show (maximum offs)) (maximum offs <= 5 + 1e-9)
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

-- | Информация в акцент: атака на каждом событии, повтор тише.
--
-- Слияние повторов верно для держащего голоса и неверно для щипкового: у
-- него «нет новой атаки» означает тишину, и такт слышится как две ноты и
-- пауза вместо фразы.
accentTests :: TestTree
accentTests =
  testGroup
    "информация в акцент"
    [ testCase "начала пробегов размечаются" $ do
        firstOfRun [1 :: Int, 1, 2, 2, 2, 3] @?= [True, False, True, False, False, True]
    , testCase "пустой вход" $ do
        firstOfRun ([] :: [Int]) @?= []
    , testCase "атака на каждом событии, в отличие от слияния" $ do
        onsetsIn (accentPattern 55 0.5 8 (replicate 8 12)) @?= 8
        onsetsIn (melodyPattern 55 8 (replicate 8 12)) @?= 1
    , testCase "повтор берётся тише, новое в полную силу" $ do
        let amps p = [noteAmp (eventValue e) | e <- queryArc p (Arc 0 1)]
        amps (accentPattern 55 0.4 4 [3, 3, 7, 7]) @?= [1, 0.4, 1, 0.4]
    , testCase "все разные события в полную силу" $ do
        let amps p = [noteAmp (eventValue e) | e <- queryArc p (Arc 0 1)]
        amps (accentPattern 55 0.4 4 [1, 2, 3, 4]) @?= [1, 1, 1, 1]
    ]
