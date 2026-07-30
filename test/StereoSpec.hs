-- | Стерео-слой: панорама, обработка каналов, сложение.
module StereoSpec (tests) where

import Data.Vector.Unboxed qualified as U
import Sound.Sig
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Stereo"
    [ panTests
    , orbitTests
    , mixTests
    ]

-- | Уровни каналов постоянного сигнала.
levels :: Stereo -> (Double, Double)
levels (Stereo l r) = (level l, level r)
  where
    level s = U.head (render defaultEnv (takeSec 0.01 s))

panTests :: TestTree
panTests =
  testGroup
    "pan"
    [ testCase "центр делит по 1/sqrt 2" $ do
        let (l, r) = levels (pan 0 (constant 1))
        assertBool (show (l, r)) (abs (l - 1 / sqrt 2) < 1e-12)
        assertBool (show (l, r)) (abs (r - 1 / sqrt 2) < 1e-12)
    , testCase "крайние положения отдают канал целиком" $ do
        let (l, r) = levels (pan (-1) (constant 1))
            (l', r') = levels (pan 1 (constant 1))
        assertBool (show (l, r)) (abs (l - 1) < 1e-12 && abs r < 1e-12)
        assertBool (show (l', r')) (abs l' < 1e-12 && abs (r' - 1) < 1e-12)
    , -- Смысл закона равной мощности: положение в образе не меняет громкости.
      testCase "мощность не зависит от положения" $
        let power p = let (l, r) = levels (pan p (constant 1)) in l * l + r * r
         in mapM_
              (\p -> assertBool (show (p, power p)) (abs (power p - 1) < 1e-12))
              [-1, -0.7, -0.3, 0, 0.25, 0.6, 1]
    , testCase "выход за диапазон прижимается к краю" $ do
        levels (pan (-5) (constant 1)) @?= levels (pan (-1) (constant 1))
        levels (pan 5 (constant 1)) @?= levels (pan 1 (constant 1))
    , testCase "mono это центр" $
        levels (mono (constant 1)) @?= levels (pan 0 (constant 1))
    , -- Пара несимметричная: на одинаковых каналах перестановка местами была
      -- бы неотличима от обработки.
      testCase "bothChannels обрабатывает оба канала, не путая их" $ do
        let (l, r) = levels (bothChannels (* constant 2) (Stereo (constant 1) (constant 3)))
        assertBool (show (l, r)) (abs (l - 2) < 1e-12 && abs (r - 6) < 1e-12)
    ]

-- | Углы в радианах: постоянный угол это неподвижный источник.
still :: Double -> Sig -> Stereo
still a = orbit (constant a)

-- | Энергия сигнала за отрезок.
energy :: Double -> Sig -> Double
energy secs s = U.sum (U.map (\x -> x * x) (render defaultEnv (takeSec secs s)))

-- | Сдвиг правого канала относительно левого в сэмплах: по максимуму
-- взаимной корреляции. Отрицательный означает, что правый пришёл раньше.
--
-- Источник обязан быть широкополосным: у синуса корреляция периодична, и
-- максимум находится на первом попавшемся алиасе.
lagSamples :: Stereo -> Int
lagSamples (Stereo l r) = snd (maximum [(score k, k) | k <- [-96 .. 96]])
  where
    xs = render defaultEnv (takeSec 0.2 l)
    ys = render defaultEnv (takeSec 0.2 r)
    n = U.length xs - 256
    score k = U.sum (U.zipWith (*) (U.slice 128 n xs) (U.slice (128 + k) n ys))

orbitTests :: TestTree
orbitTests =
  testGroup
    "orbit"
    [ -- Спереди уши слышат одно и то же.
      testCase "спереди каналы совпадают" $ do
        let Stereo l r = still 0 (sine 500)
            xs = render defaultEnv (takeSec 0.05 l)
            ys = render defaultEnv (takeSec 0.05 r)
        assertBool "каналы разошлись" (U.maximum (U.map abs (U.zipWith (-) xs ys)) < 1e-12)
    , -- Главный признак обхода: ближнее ухо слышит раньше. При 0.65 мс это
      -- 31 сэмпл на 48 кГц, и знак говорит, с какой стороны источник.
      testCase "ближнее ухо слышит раньше" $ do
        let right = lagSamples (still (pi / 2) (noise 0))
            left = lagSamples (still (negate pi / 2) (noise 0))
            front = lagSamples (still 0 (noise 0))
            oblique = lagSamples (still (pi / 4) (noise 0))
        assertBool (show right) (right >= -34 && right <= -29)
        assertBool (show left) (left >= 29 && left <= 34)
        front @?= 0
        -- Синус угла: на 45 градусах разница в 0.707 от полной.
        assertBool (show oblique) (oblique >= -25 && oblique <= -20)
    , -- Признака, различающего перед и зад, в модели нет, и это надо знать:
      -- полный оборот читается как обход через бока. Лечится HRTF.
      testCase "перед и зад неразличимы" $ do
        let front = render defaultEnv (takeSec 0.05 (leftChan (still 0 (noise 0))))
            back = render defaultEnv (takeSec 0.05 (leftChan (still pi (noise 0))))
        assertBool "модель различает" (U.maximum (U.map abs (U.zipWith (-) front back)) < 1e-12)
    , -- Тень головы: у дальнего уха верха меньше, чем у ближнего.
      testCase "дальнее ухо темнее ближнего" $ do
        let Stereo l r = still (pi / 2) (saw 300)
            top s = energy 0.2 (highpass 4000 s)
        assertBool (show (top l, top r)) (top l < 0.2 * top r)
    , testCase "по кругу энергия не проваливается" $ do
        let total a = let Stereo l r = still a (saw 300) in energy 0.2 l + energy 0.2 r
            angles = [0, pi / 4, pi / 2, 3 * pi / 4, pi, 5 * pi / 4, 3 * pi / 2]
            es = map total angles
        assertBool (show es) (maximum es < 2 * minimum es)
    , -- Полный оборот возвращает в ту же точку.
      testCase "оборот замыкается" $ do
        let a = still 0 (saw 300)
            b = still (2 * pi) (saw 300)
            diff (Stereo x _) (Stereo y _) =
              U.maximum (U.map abs (U.zipWith (-) (render defaultEnv (takeSec 0.05 x)) (render defaultEnv (takeSec 0.05 y))))
        assertBool (show (diff a b)) (diff a b < 1e-9)
    , -- Движение не должно щёлкать: разрывов больше, чем у самого сигнала,
      -- взяться неоткуда.
      testCase "вращение не щёлкает" $ do
        let Stereo l _ = orbit (phase 2) (sine 400)
            xs = render defaultEnv (takeSec 1 l)
            plain = render defaultEnv (takeSec 1 (sine 400))
            jump v = U.maximum (U.map abs (U.zipWith (-) (U.tail v) v))
        assertBool (show (jump xs, jump plain)) (jump xs < 2 * jump plain)
    ]

mixTests :: TestTree
mixTests =
  testGroup
    "mixStereo"
    [ testCase "складывает канал с каналом" $ do
        let (l, r) = levels (mixStereo [pan (-1) (constant 0.5), pan 1 (constant 0.25)])
        assertBool (show l) (abs (l - 0.5) < 1e-12)
        assertBool (show r) (abs (r - 0.25) < 1e-12)
    , -- Нейтральный элемент был бы бесконечным и растянул бы микс: длина
      -- суммы это длина самого длинного слагаемого, не больше.
      testCase "не растягивает микс" $ do
        let Stereo l _ = mixStereo [mono (takeSec 0.1 (constant 1)), mono (takeSec 0.2 (constant 1))]
        U.length (render defaultEnv l) @?= round (0.2 * envRate defaultEnv)
    , testCase "пустой микс пустой" $ do
        let Stereo l r = mixStereo []
        U.length (render defaultEnv l) @?= 0
        U.length (render defaultEnv r) @?= 0
    ]
