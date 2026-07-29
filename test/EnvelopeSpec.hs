-- | Огибающие: брейкпоинты, adsr, экспоненциальный спад.
module EnvelopeSpec (tests) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Vector.Unboxed qualified as U
import Sound.Sig.Core
import Sound.Sig.Envelope
import Sound.Sig.Osc (sine)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Envelope"
    [ lineTests
    , expsegTests
    , adsrTests
    , decayTests
    , noteTests
    ]

near :: String -> Double -> Double -> Double -> Assertion
near what tol expected got =
  assertBool (what <> ": ждали " <> show expected <> ", получили " <> show got) $
    abs (expected - got) <= tol

rate :: Double
rate = envRate defaultEnv

-- | Сэмпл огибающей по времени в секундах.
at :: Sig -> Double -> Double
at sig t = render defaultEnv sig U.! round (t * rate)

-- | Первый сэмпл, в том числе у бесконечного сигнала.
firstSample :: Sig -> Double
firstSample sig = U.head (render defaultEnv (takeSec (1 / rate) sig))

-- Линейные брейкпоинты ---------------------------------------------------

lineTests :: TestTree
lineTests =
  testGroup
    "line"
    [ testCase "длина до последней точки" $ do
        U.length (render defaultEnv (line [(0, 0), (1, 1)])) @?= 48000
    , testCase "линейная интерполяция" $ do
        let env = line [(0, 0), (1, 1)]
        near "начало" 1e-12 0 (at env 0)
        near "четверть" 1e-12 0.25 (at env 0.25)
        near "половина" 1e-12 0.5 (at env 0.5)
    , testCase "несколько сегментов" $ do
        let env = line [(0, 0), (0.5, 1), (1, 0)]
        near "вершина" 1e-12 1 (at env 0.5)
        near "спад" 1e-12 0.5 (at env 0.75)
    , testCase "до первой точки держит её значение" $ do
        let env = line [(0.5, 0.3), (1, 0.3)]
        near "старт" 1e-12 0.3 (at env 0)
        near "перед точкой" 1e-12 0.3 (at env 0.25)
    , testCase "совпадающие времена дают мгновенный скачок" $ do
        let env = line [(0, 0), (0, 1), (1, 1)]
        near "после скачка" 1e-12 1 (at env 0)
        near "полка" 1e-12 1 (at env 0.5)
    , testCase "пустой список даёт пустой сигнал" $ do
        runSig (line []) defaultEnv @?= []
    , testCase "одна точка в нуле даёт пустой сигнал" $ do
        runSig (line [(0, 1)]) defaultEnv @?= []
    , testCase "убывающий сегмент" $ do
        near "спад" 1e-12 0.75 (at (line [(0, 1), (1, 0)]) 0.25)
    , -- Иначе перепутанный порядок даёт молчаливый мусор.
      testCase "убывающие времена это ошибка" $ do
        r <- try (evaluate (U.length (render defaultEnv (line [(0, 0), (1, 1), (0.5, 0)]))))
        case r :: Either ErrorCall Int of
          Left _ -> pure ()
          Right _ -> assertFailure "ожидали ошибку"
    , testCase "отрицательный release ловится" $ do
        r <- try (evaluate (U.length (render defaultEnv (adsr 0.1 0.1 0.5 (-0.1) 1))))
        case r :: Either ErrorCall Int of
          Left _ -> pure ()
          Right _ -> assertFailure "ожидали ошибку"
    ]

-- Экспоненциальные сегменты ----------------------------------------------

expsegTests :: TestTree
expsegTests =
  testGroup
    "expseg"
    [ testCase "длина до последней точки" $ do
        U.length (render defaultEnv (expseg [(0, 1), (1, 0.001)])) @?= 48000
    , -- Значение идёт как v0 * (v1/v0)^x, в середине это среднее
      -- геометрическое.
      testCase "геометрическая интерполяция" $ do
        let env = expseg [(0, 1), (1, 0.0001)]
        near "начало" 1e-12 1 (at env 0)
        near "середина" 1e-9 0.01 (at env 0.5)
        near "четверть" 1e-9 0.1 (at env 0.25)
    , testCase "растущий сегмент" $ do
        near "рост" 1e-9 10 (at (expseg [(0, 1), (1, 100)]) 0.5)
    , testCase "ноль в точке это ошибка" $ do
        r <- try (evaluate (U.length (render defaultEnv (expseg [(0, 1), (1, 0)]))))
        case r :: Either ErrorCall Int of
          Left _ -> pure ()
          Right _ -> assertFailure "ожидали ошибку"
    , testCase "смена знака это ошибка" $ do
        r <- try (evaluate (U.length (render defaultEnv (expseg [(0, 1), (1, -1)]))))
        case r :: Either ErrorCall Int of
          Left _ -> pure ()
          Right _ -> assertFailure "ожидали ошибку"
    ]

-- ADSR -------------------------------------------------------------------

-- | a d s r dur из примера разд. 9, но с круглыми числами.
plain :: Sig
plain = adsr 0.1 0.2 0.5 0.1 1

adsrTests :: TestTree
adsrTests =
  testGroup
    "adsr"
    [ -- Длительность ноты плюс релиз: нота отпускается в dur, дальше хвост.
      testCase "длина это dur плюс release" $ do
        U.length (render defaultEnv plain) @?= round (1.1 * rate)
    , testCase "проходит через ключевые точки" $ do
        near "старт" 1e-12 0 (at plain 0)
        near "пик атаки" 1e-9 1 (at plain 0.1)
        near "конец спада" 1e-9 0.5 (at plain 0.3)
        near "полка" 1e-12 0.5 (at plain 0.6)
        near "начало релиза" 1e-12 0.5 (at plain 1)
        near "середина релиза" 1e-9 0.25 (at plain 1.05)
    , testCase "кончается нулём" $ do
        let xs = render defaultEnv plain
        near "хвост" 1e-3 0 (U.last xs)
    , testCase "тишины в начале нет" $ do
        let xs = render defaultEnv plain
        assertBool "второй сэмпл нулевой" (xs U.! 1 > 0)
    , -- Щелчок это разрыв: шаг огибающей не может быть больше самого
      -- крутого сегмента.
      testCase "нет разрывов" $ do
        let xs = render defaultEnv plain
            steps = U.zipWith (\a b -> abs (b - a)) xs (U.drop 1 xs)
            steepest = maximum [1 / 0.1, 0.5 / 0.2, 0.5 / 0.1] / rate
        assertBool (show (U.maximum steps)) (U.maximum steps <= steepest * 1.001)
    , -- Нота короче атаки: релиз стартует оттуда, где застала нота.
      testCase "короткая нота обрезает атаку" $ do
        let short = adsr 0.5 0.2 0.5 0.1 0.25
        U.length (render defaultEnv short) @?= round (0.35 * rate)
        near "уровень в момент отпускания" 1e-9 0.5 (at short 0.25)
        near "конец релиза" 1e-3 0 (U.last (render defaultEnv short))
    , testCase "нулевая атака не даёт скачка длиннее сэмпла" $ do
        let instant = adsr 0 0.2 0.5 0.1 1
        near "первый сэмпл" 1e-9 1 (at instant 0)
    ]

-- Экспоненциальный спад --------------------------------------------------

decayTests :: TestTree
decayTests =
  testGroup
    "expdecay"
    [ testCase "стартует с единицы" $ do
        near "начало" 1e-12 1 (firstSample (expdecay 0.1))
    , testCase "за постоянную времени падает в e раз" $ do
        let xs = samples defaultEnv (expdecay 0.1)
        near "тау" 1e-9 (exp (-1)) (xs !! round (0.1 * rate))
        near "два тау" 1e-9 (exp (-2)) (xs !! round (0.2 * rate))
    , -- Бесконечная: иначе умножение обрезало бы ноту по ней.
      testCase "бесконечная" $ do
        length (samples defaultEnv (takeSec 10 (expdecay 0.001))) @?= round (10 * rate)
    , testCase "нулевая постоянная времени это ошибка" $ do
        r <- try (evaluate (firstSample (expdecay 0)))
        case r :: Either ErrorCall Double of
          Left _ -> pure ()
          Right _ -> assertFailure "ожидали ошибку"
    ]

-- Критерий M2 ------------------------------------------------------------

noteTests :: TestTree
noteTests =
  testGroup
    "нота"
    [ -- Критерий разд. 11: длина ноты задаётся огибающей через умножение.
      testCase "длина ноты ровно по огибающей" $ do
        let note = sine 440 * adsr 0.01 0.1 0.7 0.05 0.5
        U.length (render defaultEnv note) @?= round (0.55 * rate)
    , testCase "края ноты без щелчка" $ do
        let note = render defaultEnv (sine 440 * adsr 0.01 0.1 0.7 0.05 0.5)
        near "первый сэмпл" 1e-12 0 (U.head note)
        near "последний сэмпл" 1e-3 0 (U.last note)
    , testCase "нота не молчит" $ do
        let note = render defaultEnv (sine 440 * adsr 0.01 0.1 0.7 0.05 0.5)
        assertBool "тишина" (U.maximum (U.map abs note) > 0.9)
    ]
