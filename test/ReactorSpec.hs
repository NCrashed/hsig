-- | Реактор: вращение образа и порядок вспышек.
module ReactorSpec (tests) where

import Data.Vector.Unboxed qualified as U
import Reactor (reactor, rod)
import Sound.Sig
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Reactor"
    [ rodTests
    , reactorTests
    ]

rate :: Double
rate = envRate defaultEnv

-- | Среднеквадратичное по окнам: огибающая, по которой видно и вспышки, и
-- перекладку по каналам.
windowsOf :: Double -> Double -> Sig -> [Double]
windowsOf secs win s = go (render defaultEnv (takeSec secs s))
  where
    n = round (win * rate)
    go v
      | U.length v < n = []
      | otherwise = rms (U.take n v) : go (U.drop n v)
    rms v = sqrt (U.sum (U.map (\x -> x * x) v) / fromIntegral (U.length v))

-- | Число вспышек: сколько раз огибающая поднимается выше половины пика.
bursts :: [Double] -> Int
bursts env = length (filter id (zipWith (\a b -> not a && b) (False : loud) loud))
  where
    loud = map (> 0.5 * maximum env) env

rodTests :: TestTree
rodTests =
  testGroup
    "стержень"
    [ -- Ради чего всё: образ обязан обойти круг, то есть баланс каналов
      -- проходит через оба края, а через центр - на каждом полуобороте.
      -- Два оборота начиная с фронта дают три перехода: на 1, 2 и 3 секундах.
      testCase "образ обходит круг" $ do
        let Stereo l r = rod 0.5 16 440 0 0
            bal =
              [ (a - b) / max 1e-12 (a + b)
              | (a, b) <- zip (windowsOf 4 0.05 l) (windowsOf 4 0.05 r)
              ]
            -- Около центра баланс шумит, поэтому переходы считаем по
            -- уверенным окнам.
            signs = map (> 0) (filter ((> 0.05) . abs) bal)
        assertBool (show (maximum bal, minimum bal)) (maximum bal > 0.2 && minimum bal < (-0.2))
        length (filter id (zipWith (/=) signs (drop 1 signs))) @?= 3
    , -- Неподвижный стержень вспыхивает ровно столько раз, сколько заказано.
      -- Считаем по 0.9 секунды: вспышки идут с нулевого момента, и ровно
      -- секунда захватила бы начало пятой.
      testCase "вспышки идут с заданной частотой" $ do
        let Stereo l _ = rod 0 4 440 0 0
        bursts (windowsOf 0.9 0.01 l) @?= 4
    , -- Сдвиг фазы вспышки разводит стержни по времени: в пике одного сосед
      -- молчит.
      testCase "стержни вспыхивают по очереди" $ do
        let env flash = windowsOf 1 0.01 (leftChan (rod 0 3 440 0 flash))
            first = env 0
            second = env (1 / 3)
            top = snd (maximum (zip first [0 :: Int ..]))
        assertBool (show (second !! top, maximum second)) (second !! top < 0.1 * maximum second)
    , testCase "конечные значения и без клиппинга" $ do
        let Stereo l r = rod 0.5 4 440 0 0
            check s =
              let v = render defaultEnv (takeSec 1 s)
               in U.all (\x -> not (isNaN x) && not (isInfinite x) && abs x < 1) v
        assertBool "левый" (check l)
        assertBool "правый" (check r)
    , testCase "не зависит от размера блока" $ do
        let Stereo l _ = rod 0.5 4 440 0 0
            big = render defaultEnv (takeSec 0.3 l)
            small = render defaultEnv {envBlock = 61} (takeSec 0.3 l)
        assertBool "разошлось" (U.maximum (U.map abs (U.zipWith (-) big small)) < 1e-12)
    ]

-- | Баланс каналов на каждой вспышке за один оборот, 24 штуки.
--
-- Оборот берём быстрый: структура вспышек от скорости не зависит, а рендер
-- вчетверо короче.
flashBalance :: Double -> [Double]
flashBalance spread =
  [ (a - b) / max 1e-12 (a + b)
  | (a, b) <- zip (windowsOf turn flash l) (windowsOf turn flash r)
  ]
  where
    Stereo l r = reactor (1 / turn) (4 / turn) spread [440, 523.25, 659.25]
    turn = 0.5
    flash = turn / 24

-- | Глубина обхода: амплитуда цикла, укладывающегося ровно в оборот.
-- Разбросанные без порядка вспышки в этот бин не попадают.
cycleDepth :: [Double] -> Double
cycleDepth bal = 2 * sqrt (c * c + s * s) / n
  where
    n = fromIntegral (length bal)
    ang k = 2 * pi * fromIntegral k / n
    c = sum (zipWith (\k v -> v * cos (ang k)) [0 :: Int ..] bal)
    s = sum (zipWith (\k v -> v * sin (ang k)) [0 :: Int ..] bal)

reactorTests :: TestTree
reactorTests =
  testGroup
    "сборка"
    [ -- Три стержня это сумма трёх, а не один громче.
      testCase "складывает стержни" $ do
        let Stereo l _ = reactor 0.5 4 0.1 [440, 523.25, 659.25]
            Stereo a _ = rod 0.5 4 440 0 0
            Stereo b _ = rod 0.5 4 523.25 0.1 (1 / 3)
            Stereo c _ = rod 0.5 4 659.25 0.2 (2 / 3)
            got = render defaultEnv (takeSec 0.5 l)
            want = render defaultEnv (takeSec 0.5 (a + b + c))
        assertBool "не сумма" (U.maximum (U.map abs (U.zipWith (-) got want)) < 1e-12)
    , -- Главное свойство сборки. При spread = 0 стержни летят вместе и
      -- вспыхивают по очереди, поэтому вспышки обходят голову ровным шагом:
      -- 24 вспышки на оборот, баланс проходит ровно один цикл, минимум на
      -- четверти круга (источник справа), максимум на трёх четвертях.
      testCase "вспышки обходят круг ровным шагом" $ do
        let bal = flashBalance 0
            signs = map (> 0) (filter ((> 0.05) . abs) bal)
            top = snd (maximum (zip bal [0 :: Int ..]))
            bottom = snd (minimum (zip bal [0 :: Int ..]))
        length bal @?= 24
        assertBool (show bal) (maximum bal > 0.15 && minimum bal < (-0.15))
        length (filter id (zipWith (/=) signs (drop 1 signs))) @?= 1
        assertBool (show bottom) (bottom >= 4 && bottom <= 8)
        assertBool (show top) (top >= 16 && top <= 20)
    , -- Разведённые поровну стержни гасят друг друга: усиления трёх точек,
      -- разнесённых на 120 градусов, взаимно компенсируются, и обхода не
      -- слышно. Это и есть причина держать spread нулевым.
      testCase "разведённые поровну стержни гасят обход" $ do
        let together = cycleDepth (flashBalance 0)
            apart = cycleDepth (flashBalance (1 / 3))
        assertBool (show (together, apart)) (apart < 0.25 * together)
    , testCase "пустой список это тишина" $ do
        let Stereo l _ = reactor 0.5 4 0 []
        U.length (render defaultEnv (takeSec 0.1 l)) @?= 0
    ]
