{-# LANGUAGE OverloadedStrings #-}

-- | Мини-нотация строками.
module MiniSpec (tests) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.List (isInfixOf, sortOn)
import Data.Ratio ((%))
import Data.String (fromString)
import Sound.Sig.Score
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Mini"
    [ atomTests
    , noteTests
    , groupTests
    , errorTests
    ]

q :: Pattern a -> (Time, Time) -> [Event a]
q p (s, e) = queryArc p (Arc s e)

-- | Значения и границы целых отрезков в порядке времени.
shape :: Pattern String -> (Time, Time) -> [(String, (Time, Time))]
shape p arc =
  [ (eventValue e, maybe (0, 0) (\(Arc s t) -> (s, t)) (eventWhole e))
  | e <- sortOn (arcStart . eventPart) (q p arc)
  ]

atomTests :: TestTree
atomTests =
  testGroup
    "атомы"
    [ testCase "одно слово на цикл" $ do
        shape "bd" (0, 1) @?= [("bd", (0, 1))]
    , testCase "последовательность делит цикл" $ do
        shape "bd sn" (0, 1) @?= [("bd", (0, 1 % 2)), ("sn", (1 % 2, 1))]
    , testCase "тильда это пауза" $ do
        shape "bd ~ sn ~" (0, 1)
          @?= [("bd", (0, 1 % 4)), ("sn", (1 % 2, 3 % 4))]
    , testCase "звёздочка ускоряет" $ do
        shape "bd*4" (0, 1)
          @?= [ ("bd", (0, 1 % 4))
              , ("bd", (1 % 4, 1 % 2))
              , ("bd", (1 % 2, 3 % 4))
              , ("bd", (3 % 4, 1))
              ]
    , testCase "ускорение внутри слота" $ do
        shape "bd sn*2" (0, 1)
          @?= [("bd", (0, 1 % 2)), ("sn", (1 % 2, 3 % 4)), ("sn", (3 % 4, 1))]
    , testCase "дробное ускорение" $ do
        shape "bd*1.5" (0, 1) @?= [("bd", (0, 2 % 3)), ("bd", (2 % 3, 4 % 3))]
    , testCase "слэш замедляет" $ do
        shape "bd/2" (0, 1) @?= [("bd", (0, 2))]
    , -- Вопрос это прореживание на половину: за четыре цикла из 64 событий
      -- должна пропасть примерно половина.
      testCase "вопрос прореживает" $ do
        let n = length (q ("bd*16?" :: Pattern String) (0, 4))
        assertBool (show n) (n > 16 && n < 48)
    , -- У каждого вопроса свой поток случайности, как в Tidal. С общим
      -- потоком два слоя пропадали бы в такт, слой в слой.
      testCase "у каждого вопроса свой поток" $ do
        let times v = [arcStart (eventPart e) | e <- q ("bd*16?, sn*16?" :: Pattern String) (0, 4), eventValue e == v]
            a = times "bd"
            b = times "sn"
        assertBool "оба слоя пусты" (not (null a) && not (null b))
        assertBool (show (length a, length b)) (a /= b)
    , -- Явная доля прореживания, как в Tidal. Ноль оставляет всё, единица
      -- убирает всё.
      testCase "вопрос принимает долю" $ do
        let n src = length (q (fromString src :: Pattern String) (0, 4))
        n "bd*16?0.0" @?= 64
        n "bd*16?1.0" @?= 0
        assertBool "доля не влияет" (n "bd*16?0.2" > n "bd*16?0.8")
    , -- Долю опознаём только когда она прижата к вопросу: с пробелом это
      -- прореживание вполовину и отдельный атом. Ради этого доля и
      -- разбирается в токенизаторе, где пробел ещё виден.
      testCase "число через пробел это отдельный атом" $ do
        -- По присутствию атома судить нельзя: прореживание вполовину его как
        -- раз и выкидывает. Смотрим на деление цикла: два атома дают слоты по
        -- половине, один с долей - слот на весь цикл.
        let widths src = map (\(_, (s, e)) -> e - s) (shape (fromString src) (0, 4))
        assertBool "не половинки" (all (== 1 % 2) (widths "bd? 0.3"))
        assertBool "пусто" (not (null (widths "bd? 0.3")))
        assertBool "не целый цикл" (all (== 1) (widths "bd?0.3"))
    , -- Точка обязательна, как в Tidal: там долю читает float, который
      -- целого не принимает. Значит "bd?0" это вопрос и атом "0".
      testCase "целое после вопроса это атом, а не доля" $ do
        let widths src = map (\(_, (s, e)) -> e - s) (shape (fromString src) (0, 4))
        assertBool "не половинки" (all (== 1 % 2) (widths "bd?0"))
        assertBool "атом пропал" ("0" `elem` map fst (shape "bd?0" (0, 4)))
    , testCase "мусор после вопроса это ошибка" $ failsOn "bd?0.3.4"
    , testCase "доля вне диапазона это ошибка" $ do
        failsOn "bd?3.0"
        failsOn "bd?1.5"
    , -- Номер потока это порядок вхождения, как в Tidal. Иначе подобранный
      -- грув уезжал бы от правки соседнего слоя или лишних скобок.
      testCase "номер потока не зависит от остального текста" $ do
        let times src = [arcStart (eventPart e) | e <- q (fromString src :: Pattern String) (0, 4), eventValue e == "bd"]
            plain = times "bd*16?"
        assertBool "пусто" (not (null plain))
        times "bd*16?, sn cp" @?= plain
        times "[bd*16?]" @?= plain
    ]

noteTests :: TestTree
noteTests =
  testGroup
    "ноты"
    [ testCase "имя ноты даёт частоту" $ do
        noteHzOf "a4" @?= Just 440
        noteHzOf "a1" @?= Just 55
        assertBool "c4" (maybe False (\f -> abs (f - 261.6256) < 1e-3) (noteHzOf "c4"))
        -- Регистр не важен, диез и бемоль сходятся на одной ноте.
        noteHzOf "A4" @?= Just 440
        assertBool "cs4" (noteHzOf "c#4" == noteHzOf "db4")
        assertBool "cs4 = s" (noteHzOf "c#4" == noteHzOf "cs4")
        assertBool "c-1" (maybe False (\f -> abs (f - 8.1758) < 1e-3) (noteHzOf "c-1"))
    , -- Ради этого различения набор барабанов и возможен: имя барабана не
      -- должно случайно оказаться нотой.
      testCase "имена барабанов нотами не считаются" $
        mapM_ (\w -> noteHzOf w @?= Nothing) ["bd", "cp", "sn", "hh", "b", "e", "x", ""]
    , testCase "паттерн нот читается строкой" $ do
        let freqs src = map (noteFreq . eventValue) (q (fromString src :: Pattern Note) (0, 1))
            labels src = map (noteLabel . eventValue) (q (fromString src :: Pattern Note) (0, 1))
        assertBool "a1 c2" $
          and (zipWith (\a b -> abs (a - b) < 1e-3) (freqs "a1 c2") [55, 65.4064])
        labels "a1 c2" @?= ["a1", "c2"]
        -- Числа берутся как частоты в герцах, как в numbers.
        freqs "55 73.42" @?= [55, 73.42]
        -- Барабаны едут метками, частота остаётся нулевой.
        labels "bd*4" @?= replicate 4 "bd"
        freqs "bd*4" @?= replicate 4 0
    , -- Хвост после октавы это не нота: иначе reads принял бы "a4x" за a4 и
      -- опечатка в мелодии звучала бы верной нотой.
      testCase "мусор после октавы это не нота" $ do
        noteHzOf "a4x" @?= Nothing
        noteHzOf "c#4y" @?= Nothing
        noteHzOf "a" @?= Nothing
    , -- Единственное двусмысленное место таблицы: b это и нота си, и бемоль.
      testCase "bb4 это a#4" $ noteHzOf "bb4" @?= noteHzOf "a#4"
    , -- В параметрах патча паттерна нет, и подставить туда тишину вместо
      -- ноты значит потерять голос молча.
      testCase "noteHz падает на не-ноте" $ do
        noteHz "a4" @?= 440
        mapM_
          ( \w -> do
              r <- try (evaluate (noteHz w))
              case r :: Either ErrorCall Double of
                Left err -> assertBool (show err) (w `isInfixOf` show err)
                Right v -> assertFailure (w <> " дал " <> show v)
          )
          ["bd", "h4", "a4x", "a"]
    ]

groupTests :: TestTree
groupTests =
  testGroup
    "группы"
    [ testCase "квадратные скобки это подгруппа" $ do
        shape "[bd sn] cp" (0, 1)
          @?= [("bd", (0, 1 % 4)), ("sn", (1 % 4, 1 % 2)), ("cp", (1 % 2, 1))]
    , testCase "угловые скобки меняют по циклам" $ do
        shape "<bd sn>" (0, 2) @?= [("bd", (0, 1)), ("sn", (1, 2))]
    , testCase "угловые внутри последовательности" $ do
        shape "<bd sn> cp" (0, 1) @?= [("bd", (0, 1 % 2)), ("cp", (1 % 2, 1))]
        shape "<bd sn> cp" (1, 2) @?= [("sn", (1, 3 % 2)), ("cp", (3 % 2, 2))]
    , testCase "запятая накладывает" $ do
        shape "bd, cp" (0, 1) @?= [("bd", (0, 1)), ("cp", (0, 1))]
    , testCase "запятая внутри скобок" $ do
        shape "[bd sn, cp]" (0, 1)
          @?= [("bd", (0, 1 % 2)), ("cp", (0, 1)), ("sn", (1 % 2, 1))]
    , testCase "вложенные скобки" $ do
        shape "[[bd sn] cp]" (0, 1)
          @?= [("bd", (0, 1 % 4)), ("sn", (1 % 4, 1 % 2)), ("cp", (1 % 2, 1))]
    , testCase "пустая строка это тишина" $ do
        q ("" :: Pattern String) (0, 4) @?= []
    , testCase "числа разбираются" $ do
        map eventValue (q (numbers "55 73.42") (0, 1)) @?= [55, 73.42]
    ]

errorTests :: TestTree
errorTests =
  testGroup
    "ошибки"
    [ testCase "незакрытая квадратная скобка" $ failsOn "[bd sn"
    , testCase "незакрытая угловая скобка" $ failsOn "<bd sn"
    , testCase "лишняя закрывающая" $ failsOn "bd]"
    , testCase "звёздочка без числа" $ failsOn "bd*"
    , testCase "не число после звёздочки" $ failsOn "bd*x"
    , -- Значения ленивы, поэтому ошибка вылезает при их использовании, а не
      -- при разборе.
      testCase "не число в numbers" $ do
        r <- try (evaluate (sum (map eventValue (q (numbers "bd") (0, 1)))))
        case r :: Either ErrorCall Double of
          Left _ -> pure ()
          Right _ -> assertFailure "ожидали ошибку"
    ]

-- | Разбор строки обязан падать с ошибкой.
failsOn :: String -> Assertion
failsOn src = do
  r <- try (evaluate (length (q (parsePat src) (0, 1))))
  case r :: Either ErrorCall Int of
    Left _ -> pure ()
    Right n -> assertFailure ("разобралось в " <> show n <> " событий")
