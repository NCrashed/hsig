-- | Фазовый интегратор и синус.
module OscSpec (tests) where

import Data.Complex (Complex, magnitude)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Vector.Storable qualified as V
import Data.Vector.Unboxed qualified as U
import Numeric.FFT.Vector.Unnormalized qualified as FFT
import Sound.Sig.Core
import Sound.Sig.Osc
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Osc" [phaseTests, sineTests]

near :: String -> Double -> Double -> Double -> Assertion
near what tol expected got =
  assertBool (what ++ ": ждали " ++ show expected ++ ", получили " ++ show got) $
    abs (expected - got) <= tol

phaseTests :: TestTree
phaseTests =
  testGroup
    "phase"
    [ testCase "стартует с нуля" $ do
        take 1 (samples defaultEnv (phase 440)) @?= [0]
    , testCase "шаг равен 2*pi*f/sr" $ do
        let step = 2 * pi * 440 / envRate defaultEnv
            got = take 3 (samples defaultEnv (phase 440))
        near "первый шаг" 1e-12 step (got !! 1)
        near "второй шаг" 1e-12 (2 * step) (got !! 2)
    , testCase "приведена к [0, 2*pi)" $ do
        let xs = take 100000 (samples defaultEnv (phase 1000))
        assertBool "вне диапазона" (all (\p -> p >= 0 && p < 2 * pi) xs)
    , testCase "длина равна длине частоты" $ do
        length (samples defaultEnv (phase (takeSec 0.5 440))) @?= 24000
    , -- Перенос фазы между блоками обязан быть незаметен: иначе на стыке
      -- будет разрыв, а с ним щелчок.
      testCase "не зависит от размера блока" $ do
        let big = take 10000 (samples defaultEnv (phase 440))
            small = take 10000 (samples defaultEnv {envBlock = 64} (phase 440))
        big @?= small
    , testCase "на границе блока совпадает с аналитической" $ do
        let n = envBlock defaultEnv
            got = samples defaultEnv (phase 440) !! n
        nearPhase 1e-9 (wrapped (fromIntegral n * stepOf 440)) got
    , testCase "через минуту фаза не уплывает" $ do
        let n = 60 * round (envRate defaultEnv)
            got = samples defaultEnv (phase 440) !! n
        nearPhase 1e-5 (wrapped (fromIntegral n * stepOf 440)) got
    ]

-- | Расстояние по кругу: 2*pi - eps и 0 это одна и та же фаза.
nearPhase :: Double -> Double -> Double -> Assertion
nearPhase tol expected got =
  assertBool ("фаза: ждали " <> show expected <> ", получили " <> show got) $
    min d (2 * pi - d) <= tol
  where
    d = wrapped (got - expected)

-- | Шаг фазы за сэмпл при частоте f.
stepOf :: Double -> Double
stepOf f = 2 * pi * f / envRate defaultEnv

wrapped :: Double -> Double
wrapped p = p - 2 * pi * fromIntegral (floor (p / (2 * pi)) :: Int)

-- | Модуль спектра, ненормированный R2C.
spectrum :: U.Vector Double -> [Double]
spectrum v = map magnitude (V.toList spec)
  where
    spec = FFT.run FFT.dftR2C (V.fromList (U.toList v)) :: V.Vector (Complex Double)

sineTests :: TestTree
sineTests =
  testGroup
    "sine"
    [ testCase "амплитуда единичная" $ do
        let xs = render defaultEnv (takeSec 1 (sine 440))
        near "пик" 1e-3 1 (U.maximum xs)
        near "минимум" 1e-3 (-1) (U.minimum xs)
    , testCase "длина равна длине частоты" $ do
        length (samples defaultEnv (sine (takeSec 0.5 440))) @?= 24000
    , -- 48000 сэмплов при 48 кГц: 440 Гц попадает ровно в бин 440,
      -- поэтому утечки спектра нет и остаток должен быть на уровне
      -- ошибки Double.
      testCase "спектр чистый: всё вне 440 Гц ниже -100 dBFS" $ do
        let mags = spectrum (render defaultEnv (takeSec 1 (sine 440)))
            peak = mags !! 440
            rest = [(k, m) | (k, m) <- zip [0 :: Int ..] mags, k /= 440]
            (worstBin, worst) = maximumBy (comparing snd) rest
            db = 20 * logBase 10 (worst / peak)
        assertBool ("бин " ++ show worstBin ++ ": " ++ show db ++ " dB") (db < -100)
    , -- 440 Гц это ровно 26400 периодов за минуту, поэтому окно остаётся
      -- выровненным по бину и та же проверка ловит уже накопленный дрейф.
      testCase "спектр остаётся чистым через минуту" $ do
        let n = 60 * round (envRate defaultEnv)
            -- Поток пропускаем мимо, а не материализуем: иначе в памяти
            -- висит вся минута.
            window = U.fromListN 48000 (take 48000 (drop n (samples defaultEnv (sine 440))))
            mags = spectrum window
            peak = mags !! 440
            worst = maximum [m | (k, m) <- zip [0 :: Int ..] mags, k /= 440]
            db = 20 * logBase 10 (worst / peak)
        assertBool (show db <> " dB") (db < -100)
    , testCase "пик спектра на 440 Гц" $ do
        let mags = spectrum (render defaultEnv (takeSec 1 (sine 440)))
            best = fst (maximumBy (comparing snd) (zip [0 :: Int ..] mags))
        best @?= 440
    ]
