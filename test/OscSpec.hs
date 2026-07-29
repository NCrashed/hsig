-- | Осцилляторы: фаза, синус, аддитивные волны, алиасинг, шум.
module OscSpec (tests) where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Vector.Unboxed qualified as U
import Sound.Sig.Core
import Sound.Sig.Osc
import Spectral (spectrum)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Osc"
    [ phaseTests
    , sineTests
    , gainTests
    , waveTests
    , aliasTests
    , noiseTests
    ]

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

-- Сброс гармоник у Найквиста ---------------------------------------------

gainTests :: TestTree
gainTests =
  testGroup
    "partialGain"
    [ testCase "до половины Найквиста без изменений" $ do
        map partialGain [0, 0.1, 0.25, 0.5] @?= [1, 1, 1, 1]
    , testCase "на Найквисте и выше ноль" $ do
        map partialGain [1, 1.5, 2] @?= [0, 0, 0]
    , testCase "в середине спада половина" $ do
        near "gain(0.75)" 1e-12 0.5 (partialGain 0.75)
    , testCase "монотонно убывает" $ do
        let xs = map (partialGain . (/ 100)) [50 .. 100]
        assertBool "не монотонно" (and (zipWith (>=) xs (drop 1 xs)))
    , -- Разрыв здесь и есть тот самый щелчок на свипе.
      testCase "непрерывна на краях спада" $ do
        near "слева от 0.5" 1e-6 1 (partialGain 0.4999999)
        near "справа от 0.5" 1e-6 1 (partialGain 0.5000001)
        near "слева от 1" 1e-9 0 (partialGain 0.9999999)
    ]

-- Форма волны ------------------------------------------------------------

-- | Значение в четверти периода при 100 Гц: индекс 120 это ровно pi/2.
quarterAt :: (Sig -> Sig) -> Double
quarterAt wave = render defaultEnv (takeSec 0.0026 (wave 100)) U.! 120

waveTests :: TestTree
waveTests =
  testGroup
    "форма волны"
    [ -- Ряд пилы сходится к phi/pi, в четверти периода это 0.5. Проверка
      -- ловит и масштаб 2/pi, и знаки (-1)^(k+1).
      testCase "пила в четверти периода даёт 0.5" $ do
        near "saw" 0.02 0.5 (quarterAt saw)
    , testCase "меандр на полке даёт 1" $ do
        near "square" 0.02 1 (quarterAt square)
    , -- Недобор около 0.0023 это обрезка ряда на 240-й гармонике плюс спад
      -- верхней октавы у Найквиста, а не ошибка коэффициентов.
      testCase "треугольник в четверти периода даёт 1" $ do
        near "tri" 0.005 1 (quarterAt tri)
    , testCase "пила стартует с нуля" $ do
        near "saw[0]" 1e-12 0 (render defaultEnv (takeSec 0.001 (saw 100)) U.! 0)
    , testCase "длина равна длине частоты" $ do
        length (samples defaultEnv (saw (takeSec 0.01 100))) @?= 480
        length (samples defaultEnv (square (takeSec 0.01 100))) @?= 480
        length (samples defaultEnv (tri (takeSec 0.01 100))) @?= 480
    , testCase "нулевая частота даёт тишину" $ do
        render defaultEnv (takeSec 0.001 (saw 0)) @?= U.replicate 48 0
    , -- Зеркальная волна, а не мусор: знак учтён в фазе, число гармоник по
      -- модулю частоты.
      testCase "отрицательная частота отражает волну" $ do
        let up = render defaultEnv (takeSec 0.001 (saw 1000))
            down = render defaultEnv (takeSec 0.001 (saw (constant (-1000))))
        assertBool "не зеркало" (U.maximum (U.map abs (U.zipWith (+) up down)) < 1e-12)
    , -- Потолок maxPartials страхует от переполнения при частоте около нуля.
      testCase "частота около нуля не ломает рендер" $ do
        let xs = render defaultEnv (takeSec 0.001 (saw 1))
        assertBool "не конечные значения" (U.all (\v -> not (isNaN v) && not (isInfinite v)) xs)
        assertBool "амплитуда вылезла" (U.maximum (U.map abs xs) < 1.5)
    ]

-- Алиасинг ---------------------------------------------------------------

-- | Худший бин вне гармоник, в дБ относительно фундаментальной. Частоты
-- берём целыми, окно не нужно: гармоники попадают ровно в бины, утечки нет.
strayDb :: Int -> [Double] -> Double
strayDb m mags = 20 * logBase 10 (worst / (mags !! m))
  where
    worst = maximum [v | (k, v) <- zip [0 :: Int ..] mags, k `mod` m /= 0]

onlyHarmonics :: String -> Int -> [Double] -> Assertion
onlyHarmonics what m mags =
  assertBool (what <> ": посторонняя энергия на " <> show db <> " dB") (db < -100)
  where
    db = strayDb m mags

-- | Худшая из чётных гармоник, в дБ относительно фундаментальной. Отдельно
-- от strayDb: бины 2m, 4m кратны m и в посторонние не попадают.
evenHarmonicDb :: Int -> [Double] -> Double
evenHarmonicDb m mags = 20 * logBase 10 (maximum evens / (mags !! m))
  where
    evens = [mags !! (m * k) | k <- [2, 4 .. (24000 `div` m) - 1]]

-- | Спектр секунды сигнала при 48 кГц: бин равен 1 Гц.
secondOf :: Sig -> [Double]
secondOf sig = spectrum (render defaultEnv (takeSec 1 sig))

-- | Ожидаемая амплитуда k-й гармоники в бинах ненормированного БПФ.
expectedMag :: Double -> Double -> Int -> Double
expectedMag f amp k = abs amp * partialGain (fromIntegral k * f / 24000) * 24000

-- | Амплитуды гармоник совпадают с рядом Фурье с учётом спада у Найквиста.
assertHarmonics :: Int -> (Sig -> Sig) -> (Int -> Double) -> [Int] -> Assertion
assertHarmonics f wave amp = mapM_ check
  where
    mags = secondOf (wave (constant (fromIntegral f)))
    check k =
      let want = expectedMag (fromIntegral f) (amp k) k
       in near ("гармоника " <> show k) (0.005 * want + 1e-9) want (mags !! (f * k))

aliasTests :: TestTree
aliasTests =
  testGroup
    "алиасинг"
    [ -- Все частоты ниже (137, 1234, 7000) выбраны так, чтобы не делить
      -- 48000 нацело: у делителей завёрнутая гармоника падает ровно в бин
      -- настоящей, и проверка к заворачиванию слепа.
      testCase "пила 137 Гц: энергия только в гармониках" $
        onlyHarmonics "saw 137" 137 (secondOf (saw 137))
    , testCase "пила 1234 Гц: энергия только в гармониках" $
        onlyHarmonics "saw 1234" 1234 (secondOf (saw 1234))
    , testCase "пила 7000 Гц: энергия только в гармониках" $
        onlyHarmonics "saw 7000" 7000 (secondOf (saw 7000))
    , testCase "меандр 1234 Гц: только нечётные гармоники" $ do
        let mags = secondOf (square 1234)
        onlyHarmonics "square 1234" 1234 mags
        let db = evenHarmonicDb 1234 mags
        assertBool ("чётная гармоника на " <> show db <> " dB") (db < -100)
    , testCase "треугольник 1234 Гц: только нечётные гармоники" $ do
        let mags = secondOf (tri 1234)
        onlyHarmonics "tri 1234" 1234 mags
        let db = evenHarmonicDb 1234 mags
        assertBool ("чётная гармоника на " <> show db <> " dB") (db < -100)
    , -- Пинает и масштаб, и сброс у Найквиста: 7000 Гц это три гармоники,
      -- третья уже глубоко в спаде.
      testCase "амплитуды гармоник пилы совпадают с рядом" $
        assertHarmonics 7000 saw (\k -> (if odd k then 2 else -2) / pi / fromIntegral k) [1, 2, 3]
    , testCase "амплитуды гармоник меандра спадают как 1/k" $
        assertHarmonics 1234 square (\k -> if even k then 0 else 4 / pi / fromIntegral k) [1, 3, 5, 7, 9]
    , testCase "амплитуды гармоник треугольника спадают как 1/k^2" $
        assertHarmonics 1234 tri (\k -> if even k then 0 else 8 / (pi * pi) / fromIntegral (k * k)) [1, 3, 5, 7]
    , -- Без плавного спада уход гармоники за Найквист это скачок на 2/(pi*k).
      testCase "сброс гармоники не даёт скачка" $ do
        let edge = 4800 -- пятая гармоника ровно на Найквисте
            below = render defaultEnv (takeSec 0.01 (saw (constant (edge - 0.001))))
            above = render defaultEnv (takeSec 0.01 (saw (constant (edge + 0.001))))
            diff = U.maximum (U.map abs (U.zipWith (-) below above))
        assertBool ("расхождение " <> show diff) (diff < 1e-3)
    , -- Приёмочная проверка разд. 12 в исполнимом виде: у свипа гармоники
      -- размазаны по бинам, поэтому смотрим полосу ниже фундаментальной,
      -- где у пилы ничего нет, а завёрнутые гармоники были бы.
      testCase "свип 20-8000 Гц не заворачивает гармоники" $ do
        let xs = render defaultEnv (saw (fromSamples sweepFreqs))
        assertBool "не конечные значения" (U.all (\v -> not (isNaN v) && not (isInfinite v)) xs)
        assertBool "амплитуда вылезла" (U.maximum (U.map abs xs) < 1.3)
        mapM_
          ( \fr ->
              let db = belowFundamentalDb xs fr
               in assertBool ("кадр " <> show fr <> " на " <> show db <> " dB") (db < -80)
          )
          sweepFrames
    , -- Негативный контроль: та же проверка на наивной пиле обязана падать,
      -- иначе порог и полоса подобраны так, что не ловят ничего.
      testCase "стационарная проверка ловит наивную пилу" $ do
        let db = strayDb 7000 (spectrum (naiveSaw (replicate 48000 7000)))
        assertBool ("наивная пила прошла: " <> show db <> " dB") (db > -40)
    , testCase "свиповая проверка ловит наивную пилу" $ do
        let xs = naiveSaw sweepFreqs
            worst = maximum (map (belowFundamentalDb xs) sweepFrames)
        assertBool ("наивный свип прошёл: " <> show worst <> " dB") (worst > -40)
    ]

-- | Свип 20 - 8000 Гц за 4 секунды из разд. 12.
sweepFreqs :: [Double]
sweepFreqs = [20 + (8000 - 20) * fromIntegral i / fromIntegral n | i <- [0 .. n - 1]]
  where
    n = round (4 * envRate defaultEnv) :: Int

-- | Кадры свипа, которые эта методика способна проверить. Ниже второго
-- кадра (около 440 Гц) полоса под фундаментальной вырождается: между
-- вторым бином и защитным зазором не остаётся ничего.
sweepFrames :: [Int]
sweepFrames = [2 .. 45]

-- | Пила без оглядки на Найквист: гармоники заворачиваются. Нужна только
-- как негативный контроль для проверок выше.
naiveSaw :: [Double] -> U.Vector Double
naiveSaw freqs = U.map val (render defaultEnv (phase (fromSamples freqs)))
  where
    val p = sum [(if odd k then 2 else -2) / pi / fromIntegral k * sin (fromIntegral k * p) | k <- [1 .. 40 :: Int]]

-- | Для кадра свипа: худший бин в полосе ниже фундаментальной, в дБ.
belowFundamentalDb :: U.Vector Double -> Int -> Double
belowFundamentalDb xs frame = 20 * logBase 10 (worst / peak)
  where
    size = 4096
    piece = U.slice (frame * size) size xs
    mags = spectrum (U.zipWith (*) (blackman size) piece)
    binHz = envRate defaultEnv / fromIntegral size
    f = 20 + (8000 - 20) * (fromIntegral (frame * size) + fromIntegral size / 2) / (4 * envRate defaultEnv)
    fundBin = round (f / binHz) :: Int
    peak = maximum (take 17 (drop (fundBin - 8) mags))
    -- За кадр частота уезжает на 14 бинов, поэтому фундаментальная это не
    -- линия, а полоса шириной около 15 бинов. Верхний проверяемый бин
    -- отстоит от её центра на guard: дальше спад окна Блэкмана уводит
    -- утечку ниже -100 дБ.
    guard = 25
    worst = maximum (take (fundBin - guard - 1) (drop 2 mags))

-- | Окно Блэкмана: боковые лепестки около -58 дБ и быстрый спад.
blackman :: Int -> U.Vector Double
blackman n = U.generate n $ \i ->
  let t = 2 * pi * fromIntegral i / fromIntegral (n - 1)
   in 0.42 - 0.5 * cos t + 0.08 * cos (2 * t)

-- Шум --------------------------------------------------------------------

noiseTests :: TestTree
noiseTests =
  testGroup
    "noise"
    [ testCase "бесконечный и в диапазоне [-1, 1)" $ do
        let xs = take 10000 (samples defaultEnv (noise 0))
        assertBool "вышел за диапазон" (all (\v -> v >= -1 && v < 1) xs)
    , testCase "детерминирован по seed" $ do
        take 100 (samples defaultEnv (noise 5)) @?= take 100 (samples defaultEnv (noise 5))
    , testCase "разные seed дают разные потоки" $ do
        let a = take 1000 (samples defaultEnv (noise 0))
            b = take 1000 (samples defaultEnv (noise 1))
        assertBool "потоки совпали" (not (or (zipWith (==) a b)))
    , testCase "envSeed сдвигает поток" $ do
        let a = take 100 (samples defaultEnv (noise 0))
            b = take 100 (samples defaultEnv {envSeed = 9} (noise 0))
        assertBool "envSeed не учтён" (a /= b)
    , -- Шум индексируется номером сэмпла, иначе блочный рендер разъедется.
      testCase "не зависит от размера блока" $ do
        let a = take 500 (samples defaultEnv (noise 0))
            b = take 500 (samples defaultEnv {envBlock = 37} (noise 0))
        a @?= b
    , testCase "среднее около нуля" $ do
        let xs = take 100000 (samples defaultEnv (noise 3))
            mean = sum xs / fromIntegral (length xs)
        assertBool ("среднее " <> show mean) (abs mean < 0.01)
    , testCase "спектр плоский" $ do
        let mags = spectrum (U.fromListN 8192 (take 8192 (samples defaultEnv (noise 2))))
            bands = chunksOf 512 (map (^ (2 :: Int)) (drop 1 mags))
            power = map (\b -> sum b / fromIntegral (length b)) bands
        assertBool (show power) (maximum power / minimum power < 2)
    ]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t
