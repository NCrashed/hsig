-- | Проектирование FIR и оверсэмплинг.
module ResampleSpec (tests) where

import Data.Vector.Unboxed qualified as U
import Sound.Sig.Core
import Sound.Sig.Osc (noise, sine)
import Sound.Sig.Resample
import Spectral (rms, spectrum, windowed)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Resample"
    [ besselTests
    , firTests
    , oversampleTests
    , resampleTests
    ]

resampleTests :: TestTree
resampleTests =
  testGroup
    "resample"
    [ -- Та же частота это тождество: ядро вырождается в дельту.
      testCase "своя частота ничего не меняет" $ do
        let src = takeSec 0.05 (sine 1000)
            got = render defaultEnv (resample rate src)
            want = render defaultEnv src
        U.length got @?= U.length want
        assertBool "разошлось" (U.maximum (U.map abs (U.zipWith (-) got want)) < 1e-12)
    , testCase "длина меняется по отношению частот" $ do
        let len f = U.length (render defaultEnv (resample f (takeSec 0.1 (sine 500))))
        -- Вход считается на своей частоте, поэтому 0.1 секунды остаются
        -- 0.1 секунды: сэмплов на выходе всегда по частоте рендера.
        len (2 * rate) @?= round (0.1 * rate)
        len (rate / 2) @?= round (0.1 * rate)
    , -- Тон обязан остаться тем же тоном: время не растягивается.
      testCase "частота тона сохраняется" $ do
        let got = render defaultEnv (takeSec 1 (resample 96000 (sine 1000)))
            mags = spectrum got
            peak = snd (maximum (zip mags [0 :: Int ..]))
        peak @?= 1000
    , -- Критерий разд. 12: неравномерность в полосе пропускания меньше
      -- 0.01 дБ. Сравниваем с тем же тоном, посчитанным прямо на выходной
      -- частоте.
      testCase "полоса пропускания ровная" $ do
        let level s = rms (U.drop 4800 (render defaultEnv (takeSec 0.3 s)))
            devDb f =
              20 * logBase 10 (level (resample 96000 (sine (constant f))) / level (sine (constant f)))
        mapM_
          (\f -> assertBool (show (f, devDb f)) (abs (devDb f) < 0.01))
          [100, 1000, 5000, 10000, 15000, 20000]
    , -- Второй критерий разд. 12: затухание в полосе задерживания ниже
      -- -120 дБ. Ради него в ядре и сжатие полосы: то, что не влезает в
      -- новую полосу, обязано пропасть, а не завернуться.
      testCase "при понижении нет заворота" $ do
        let level f = rms (U.drop 4800 (render defaultEnv (takeSec 0.3 (resample 96000 (sine (constant f))))))
            db f = 20 * logBase 10 (level f / level 1000)
        mapM_
          (\f -> assertBool (show (f, db f)) (db f < -120))
          [28000, 30000, 36000, 45000]
    , -- И обратно: подъём не должен рождать образов выше исходной полосы.
      testCase "при повышении нет ничего сверху" $ do
        let mags = spectrum (render defaultEnv (takeSec 1 (resample 8000 (sine 1000))))
            peak = maximum mags
            above = maximum [m | (k, m) <- zip [0 :: Int ..] mags, k > 4200]
        assertBool (show (peak, above)) (above < 1e-3 * peak)
    , testCase "постоянная составляющая сохраняется" $ do
        let got = render defaultEnv (resample 96000 (takeSec 0.05 (constant 0.5)))
            mid = U.slice 200 (U.length got - 400) got
        assertBool (show (U.minimum mid, U.maximum mid)) $
          U.all (\v -> abs (v - 0.5) < 1e-6) mid
    , testCase "не зависит от размера блока" $ do
        let s = resample 96000 (takeSec 0.05 (sine 1000))
            big = render defaultEnv s
            small = render defaultEnv {envBlock = 61} s
        big @?= small
    ]

near :: String -> Double -> Double -> Double -> Assertion
near what tol expected got =
  assertBool (what <> ": ждали " <> show expected <> ", получили " <> show got) $
    abs (expected - got) <= tol

rate :: Double
rate = envRate defaultEnv

-- Бессель ------------------------------------------------------------------

besselTests :: TestTree
besselTests =
  testGroup
    "besselI0"
    [ -- Табличные значения: ряд обязан сходиться к ним, иначе окно Кайзера
      -- будет не тем, за что себя выдаёт.
      testCase "совпадает с таблицей" $ do
        near "I0(0)" 1e-12 1 (besselI0 0)
        near "I0(1)" 1e-9 1.2660658777520084 (besselI0 1)
        near "I0(2)" 1e-9 2.2795853023360673 (besselI0 2)
        near "I0(5)" 1e-7 27.239871823604442 (besselI0 5)
    , -- Ряд обязан сходиться и на больших аргументах: beta для 160 дБ это
      -- почти 17.
      testCase "сходится на большом аргументе" $ do
        let v = besselI0 16
        assertBool (show v) (not (isNaN v) && v > 8e5 && v < 9e5)
    ]

-- Проектирование FIR --------------------------------------------------------

-- | Отклик фильтра по его коэффициентам, дополненным нулями до n точек.
responseOf :: Int -> U.Vector Double -> [Double]
responseOf n h = spectrum (h U.++ U.replicate (n - U.length h) 0)

-- | Отклик в дБ на отрезке нормированных частот.
bandDb :: (Double, Double) -> [Double] -> [Double]
bandDb (from, to) mags =
  [ 20 * logBase 10 m
  | (k, m) <- zip [0 :: Int ..] mags
  , let f = fromIntegral k / fromIntegral fftLen
  , f >= from
  , f <= to
  ]

firTests :: TestTree
firTests =
  testGroup
    "kaiserLowpass"
    [ testCase "beta по затуханию" $ do
        near "120 дБ" 1e-6 12.26526 (kaiserBeta 120)
        near "160 дБ" 1e-4 16.67326 (kaiserBeta 160)
    , testCase "симметричен, то есть линеен по фазе" $ do
        let h = kaiserLowpass 129 0.25 160
            n = U.length h
        assertBool "не симметричен" $
          and [abs (h U.! i - h U.! (n - 1 - i)) < 1e-18 | i <- [0 .. n `div` 2]]
    , testCase "усиление на постоянном токе единица" $ do
        near "сумма отводов" 1e-12 1 (U.sum (kaiserLowpass 129 0.25 160))
    , -- Требование разд. 12: неравномерность в полосе пропускания меньше
      -- 0.01 дБ, затухание в полосе задерживания ниже -120 дБ. Фильтр тот
      -- же, что берёт oversample 8.
      testCase "полоса пропускания ровная" $ do
        let ripple = maximum (map abs (bandDb (0, 0.045) (responseOf fftLen stage8)))
        assertBool (show ripple <> " dB") (ripple < 0.01)
    , testCase "полоса задерживания глубокая" $ do
        let worst = maximum (bandDb (0.08, 0.5) (responseOf fftLen stage8))
        assertBool (show worst <> " dB") (worst < -120)
    ]

fftLen :: Int
fftLen = 16384

-- | Ровно тот фильтр, что стоит в тракте oversample 8.
stage8 :: U.Vector Double
stage8 = stageFilter 8

-- Оверсэмплинг --------------------------------------------------------------

-- | Энергия спектра в полосе до доли Найквиста. Окно обязательно: разность
-- сосредоточена у Найквиста, где фильтр законно режет, и без окна её утечка
-- залила бы измеряемую полосу.
bandEnergy :: Double -> U.Vector Double -> Double
bandEnergy upTo xs = sum [m * m | (k, m) <- zip [0 :: Int ..] mags, fromIntegral k <= edge]
  where
    mags = windowed xs
    edge = upTo * fromIntegral (U.length xs) / 2

-- | Отрезаем края: у краёв FIR ещё разгоняется.
inner :: U.Vector Double -> U.Vector Double
inner v = U.slice 128 (U.length v - 256) v

oversampleTests :: TestTree
oversampleTests =
  testGroup
    "oversample"
    [ testCase "кратность 1 ничего не меняет" $ do
        let x = takeSec 0.01 (sine 1000)
            got = render defaultEnv (oversample 1 (mapSig (* 2)) x)
            want = render defaultEnv (mapSig (* 2) x)
        assertBool "разошлось" (U.maximum (U.map abs (U.zipWith (-) got want)) < 1e-15)
    , testCase "сохраняет длину" $ do
        U.length (render defaultEnv (oversample 8 id (takeSec 0.01 (sine 1000)))) @?= 480
    , -- Критерий разд. 11 и приёмочная проверка разд. 12: oversample 8 id
      -- против id, разность ниже -140 dBFS в полосе до 0.4 Найквиста.
      testCase "null-тест: прозрачен на шуме" $ do
        let x = takeSec (8192 / rate) (noise 0)
            src = render defaultEnv x
            got = render defaultEnv (oversample 8 id x)
            diff = inner (U.zipWith (-) got src)
            ratio = bandEnergy 0.4 diff / bandEnergy 0.4 (inner src)
            db = 10 * logBase 10 ratio
        assertBool (show db <> " dB") (db < -140)
    , testCase "не сдвигает сигнал во времени" $ do
        let x = takeSec 0.01 (sine 1000)
            got = render defaultEnv (oversample 8 id x)
            src = render defaultEnv x
            err = U.maximum (U.map abs (U.zipWith (-) (inner got) (inner src)))
        assertBool (show err) (err < 1e-7)
    , -- Импульс обязан остаться на своём месте: без компенсации задержки
      -- параллельные ветви графа разъедутся.
      testCase "импульс остаётся на своём индексе" $ do
        let x = fromSamples (replicate 200 0 <> [1] <> replicate 300 0)
            got = render defaultEnv (oversample 8 id x)
            peakAt = U.maxIndex (U.map abs got)
        peakAt @?= 200
    , -- Сигнал длиной в несколько блоков при обоих размерах: иначе тест
      -- не пересёк бы ни одной границы чанка и ничего бы не проверил.
      testCase "не зависит от размера блока" $ do
        let x = takeSec 0.2 (sine 1000)
            big = render defaultEnv (oversample 8 id x)
            small = render defaultEnv {envBlock = 512} (oversample 8 id x)
        U.length big @?= 9600
        assertBool "разошлось" (U.maximum (U.map abs (U.zipWith (-) big small)) < 1e-15)
    , -- Внутренняя обработка обязана идти на повышенной частоте. Синус,
      -- рождённый внутри, после децимации должен остаться на своей частоте;
      -- если envRate не домножить, он уедет в n раз.
      testCase "внутри работает повышенная частота дискретизации" $ do
        let x = takeSec (8192 / rate) (constant 1)
            got = render defaultEnv (oversample 8 (\s -> s * sine 1000) x)
            mags = windowed (inner got)
            peak = snd (maximum [(m, k) | (k, m) <- zip [0 :: Int ..] mags])
            hz = fromIntegral peak * rate / fromIntegral (U.length (inner got))
        assertBool (show hz <> " Гц") (abs (hz - 1000) < 20)
    , -- Разд. 6.4 требует умножать на кратность и размер блока. На звук это
      -- не влияет (выравнивание блоков всё стерпит), поэтому проверяем
      -- зондом, который сообщает свой envBlock значением.
      testCase "внутри блок умножен на кратность" $ do
        let probe = Sig (\env -> repeat (U.replicate (blockOf env) (fromIntegral (envBlock env))))
            got = render defaultEnv (oversample 8 (* probe) (takeSec 0.05 (constant 1)))
        near "блок" 1 (fromIntegral (envBlock defaultEnv * 8)) (got U.! 1000)
    , testCase "работает на бесконечном сигнале" $ do
        let xs = take 2000 (samples defaultEnv (oversample 8 id (constant 1)))
            tailOf = drop 500 xs
        assertBool "не единица" (all (\v -> abs (v - 1) < 1e-6) tailOf)
    , -- Вложенность: внутренний oversample видит уже поднятый Env и
      -- компенсирует свою задержку в его сэмплах.
      testCase "вложенный oversample прозрачен" $ do
        let x = takeSec 0.05 (sine 1000)
            got = render defaultEnv (oversample 2 (oversample 4 id) x)
            src = render defaultEnv x
            trim v = U.slice 400 (U.length v - 800) v
            err = U.maximum (U.map abs (U.zipWith (-) (trim got) (trim src)))
        assertBool (show err) (err < 1e-6)
    , testCase "линейная обработка внутри проходит насквозь" $ do
        let x = takeSec 0.01 (sine 1000)
            got = render defaultEnv (oversample 4 (mapSig (* 2)) x)
            want = U.map (* 2) (render defaultEnv x)
            err = U.maximum (U.map abs (U.zipWith (-) (inner got) (inner want)))
        assertBool (show err) (err < 1e-6)
    ]
