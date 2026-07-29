-- | Приёмка патча из разд. 9: критерий там субъективный, поэтому меряем
-- то, что отличает плотный звук от грязного.
module LeadSpec (tests) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.List (sortOn)
import Data.Vector.Unboxed qualified as U
import Lead (lead, leadWide, unison, unisonVoices, unisonWide)
import Sound.Sig
import Spectral (spectrum)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Lead"
    [ unisonTests
    , wideTests
    , patchTests
    ]

rms :: U.Vector Double -> Double
rms v = sqrt (U.sum (U.map (\x -> x * x) v) / fromIntegral (U.length v))

rmsOf :: Double -> Sig -> Double
rmsOf secs s = rms (render defaultEnv (takeSec secs s))

-- | Ширина образа: отношение среднеквадратичных side и mid. Ноль это моно.
width :: Double -> Stereo -> Double
width secs (Stereo l r) = rms (side (-)) / rms (side (+))
  where
    xs = render defaultEnv (takeSec secs l)
    ys = render defaultEnv (takeSec secs r)
    side f = U.zipWith (\a b -> f a b / 2) xs ys

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
    , testCase "голосов ровно столько, сколько заказано" $
        length (unisonVoices 7 18 440) @?= 7
    , -- Стек без голосов это ошибка вызывающего, а не тишина и не NaN от
      -- деления на число голосов.
      testCase "стек без голосов это ошибка" $ do
        let force s = U.sum (render defaultEnv (takeSec 0.01 s))
        mono' <- try (evaluate (force (unison 0 0 440)))
        wide <- try (evaluate (let Stereo l _ = unisonWide 0 0 440 in force l))
        case (mono' :: Either ErrorCall Double, wide :: Either ErrorCall Double) of
          (Left _, Left _) -> pure ()
          other -> assertFailure (show other)
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

wideTests :: TestTree
wideTests =
  testGroup
    "широкий стек"
    [ -- Ради этого он и нужен: у моно-стека ширина ровно нулевая, у
      -- разведённого по панораме она сравнима с самим сигналом (голоса
      -- расстроены, значит между каналами не сокращаются).
      testCase "стек даёт ширину" $ do
        -- Ровно нуля тут не бывает: cos и sin от pi/4 расходятся на 1 ulp.
        assertBool "моно не по центру" (width 1 (mono (unison 7 18 440)) < 1e-12)
        let w = width 1 (unisonWide 7 18 440)
        assertBool (show w) (w > 0.2)
    , testCase "один голос остаётся по центру" $ do
        let w = width 0.2 (unisonWide 1 0 440)
        assertBool (show w) (w < 1e-12)
    , -- Совместимость с моно. Побитово сумма каналов моно-стеку не равна и
      -- не может: закон равной мощности поднимает центр в sqrt 2 раза, а
      -- края лишь в 1.075, поэтому в сумме крайние голоса тише на четверть.
      -- Важно другое: усиления все положительные, значит голоса не
      -- вычитаются и стек в моно не проваливается.
      testCase "стек не проваливается в моно" $ do
        let Stereo l r = unisonWide 7 18 440
            sumC = render defaultEnv (takeSec 1 ((l + r) * constant (1 / sqrt 2)))
            monoC = render defaultEnv (takeSec 1 (unison 7 18 440))
            ratio = rms sumC / rms monoC
        assertBool (show ratio) (ratio > 0.8 && ratio < 1.02)
    , -- Метрика ширины к перекосу образа слепа: смещённое с центра моно даёт
      -- её не хуже честной разводки. Перекос виден только по балансу.
      testCase "каналы сбалансированы" $ do
        let Stereo l r = unisonWide 7 18 440
            ratio = rmsOf 1 l / rmsOf 1 r
        assertBool (show ratio) (abs (ratio - 1) < 0.05)
    , -- Инвариант, на котором держится замена моно-лида парой хард-панорамных
      -- стемов: канал широкого стека по уровню равен каналу моно-стека,
      -- поставленного по центру. Сумма cos^2 по симметричным положениям
      -- равна n/2, отсюда и равенство.
      testCase "уровень канала как у центрированного моно" $ do
        let Stereo l _ = unisonWide 7 18 440
            Stereo c _ = mono (unison 7 18 440)
            ratio = rmsOf 1 l / rmsOf 1 c
        assertBool (show ratio) (abs (ratio - 1) < 0.1)
    , -- Патч поверх широкого стека обязан остаться патчем: слот, уровень,
      -- отсутствие постоянной составляющей.
      testCase "патч на широком стеке ведёт себя как патч" $ do
        let Stereo l r = leadWide (noteOf 440) {noteDur = 0.5}
            xs = render defaultEnv l
            ys = render defaultEnv r
            mean v = U.sum v / fromIntegral (U.length v)
        U.length xs @?= round (0.5 * rate)
        U.length ys @?= round (0.5 * rate)
        assertBool (show (U.maximum (U.map abs xs))) (U.maximum (U.map abs xs) < 1)
        assertBool (show (U.maximum (U.map abs ys))) (U.maximum (U.map abs ys) < 1)
        assertBool (show (mean xs)) (abs (mean xs) < 1e-3)
        assertBool (show (mean ys)) (abs (mean ys) < 1e-3)
    , testCase "патч сохраняет ширину" $ do
        let w = width 0.5 (leadWide (noteOf 440) {noteDur = 0.5})
        assertBool (show w) (w > 0.1)
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
