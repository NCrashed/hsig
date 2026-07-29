-- | Приёмка патча из разд. 9: критерий там субъективный, поэтому меряем
-- то, что отличает плотный звук от грязного.
module LeadSpec (tests) where

import Data.List (sortOn)
import Data.Vector.Unboxed qualified as U
import Lead (lead, unison)
import Sound.Sig
import Spectral (spectrum)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Lead"
    [ unisonTests
    , patchTests
    ]

rate :: Double
rate = envRate defaultEnv

-- | Нота патча длиной dur на частоте f.
noteAt :: Double -> Double -> U.Vector Double
noteAt f dur = render defaultEnv (lead (noteOf f) {noteDur = dur})

unisonTests :: TestTree
unisonTests =
  testGroup
    "unison"
    [ -- Один голос без расстройки это почти пила: дрейф остаётся и на нём,
      -- но за сотую долю секунды уводит фазу лишь на единицы 1e-5.
      testCase "один голос идёт за пилой" $ do
        let a = render defaultEnv (takeSec 0.01 (unison 1 0 440))
            b = render defaultEnv (takeSec 0.01 (saw 440))
            diff = U.maximum (U.map abs (U.zipWith (-) a b))
        assertBool (show diff) (diff < 1e-3)
    , -- Расстройка обязана давать несколько пиков вокруг основной частоты,
      -- иначе стек звучит как один голос.
      testCase "расстройка разводит голоса" $ do
        let xs = render defaultEnv (takeSec 1 (unison 7 18 440))
            mags = spectrum xs
            near440 = [(k, m) | (k, m) <- zip [0 :: Int ..] mags, k > 430, k < 450]
            strong = length [() | (_, m) <- near440, m > 0.05 * maximum (map snd near440)]
        assertBool (show strong) (strong >= 5)
    , testCase "не разгоняет амплитуду" $ do
        let xs = render defaultEnv (takeSec 0.5 (unison 7 18 220))
        assertBool (show (U.maximum (U.map abs xs))) (U.maximum (U.map abs xs) < 1.2)
    , -- Дрейф проверяем на одном голосе против чистой пилы: расстройка тут
      -- нулевая, поэтому разойтись они могут только из-за него. За пять
      -- секунд LFO на 0.05 Гц уводит фазу почти на период.
      testCase "дрейф уводит фазу" $ do
        let a = render defaultEnv (takeSec 5 (unison 1 0 440))
            b = render defaultEnv (takeSec 5 (saw 440))
            diff = U.maximum (U.map abs (U.zipWith (-) a b))
        assertBool (show diff) (diff > 0.5)
    , -- А на длинной ноте расходятся и сами голоса стека.
      testCase "стек не застывает" $ do
        let xs = render defaultEnv (takeSec 1 (unison 7 18 440))
            period = round (rate / 440) :: Int
            early = U.slice 1000 period xs
            late = U.slice (U.length xs - period - 1000) period xs
        assertBool "сигнал застыл" (U.maximum (U.map abs (U.zipWith (-) early late)) > 0.01)
    ]

patchTests :: TestTree
patchTests =
  testGroup
    "патч"
    [ -- Нота обязана укладываться в свой слот: иначе её хвост лёг бы на
      -- атаку следующей на каждом переходе.
      testCase "нота укладывается в слот" $ do
        U.length (noteAt 440 0.5) @?= round (0.5 * rate)
        assertBool "длиннее слота" (U.length (noteAt 440 0.25) <= round (0.25 * rate))
        assertBool "длиннее слота" (U.length (noteAt 440 1) <= round (1 * rate))
    , testCase "не клиппует при единичной амплитуде" $ do
        let xs = noteAt 440 0.5
        assertBool (show (U.maximum (U.map abs xs))) (U.maximum (U.map abs xs) < 1)
    , testCase "конечные значения" $ do
        let xs = noteAt 440 0.5
        assertBool "NaN" (U.all (\v -> not (isNaN v) && not (isInfinite v)) xs)
    , -- Асимметричный шейпер даёт постоянную составляющую, гребёнка усилила
      -- бы её в 2.5 раза. Её убирает блокиратор в цепочке.
      testCase "нет постоянной составляющей" $ do
        let xs = noteAt 440 0.5
            mean = U.sum xs / fromIntegral (U.length xs)
        assertBool (show mean) (abs mean < 1e-3)
    , testCase "края без щелчка" $ do
        let xs = noteAt 440 0.5
        assertBool (show (U.head xs)) (abs (U.head xs) < 1e-9)
        assertBool (show (U.last xs)) (abs (U.last xs) < 1e-3)
    , -- Грязь от заворачивающихся гармоник села бы широкой полосой наверху.
      -- Оверсэмплинг вокруг фильтра и шейпера её не пускает.
      testCase "верх спектра чистый" $ do
        let xs = noteAt 440 1
            win = U.slice (round (0.2 * rate)) 16384 xs
            mags = spectrum win
            binHz k = fromIntegral k * rate / 16384
            band lo hi = sum [m * m | (k, m) <- zip [0 :: Int ..] mags, binHz k >= lo, binHz k < hi]
            top = band 16000 (rate / 2) / band 0 (rate / 2)
        assertBool (show top) (top < 1e-3)
    , -- Слой с decimate подмешан, но не забивает основной.
      testCase "основная энергия в музыкальной полосе" $ do
        let xs = noteAt 440 1
            win = U.slice (round (0.2 * rate)) 16384 xs
            mags = spectrum win
            binHz k = fromIntegral k * rate / 16384
            band lo hi = sum [m * m | (k, m) <- zip [0 :: Int ..] mags, binHz k >= lo, binHz k < hi]
            mid = band 100 5000 / band 0 (rate / 2)
        assertBool (show mid) (mid > 0.9)
    , testCase "самые сильные бины вокруг основной частоты" $ do
        let xs = noteAt 440 1
            win = U.slice (round (0.2 * rate)) 16384 xs
            mags = spectrum win
            binHz k = fromIntegral k * rate / 16384
            best = take 4 (sortOn (negate . snd) (zip (map binHz [0 :: Int ..]) mags))
        assertBool (show (map fst best)) (all (\(f, _) -> f > 420 && f < 460) best)
    ]
