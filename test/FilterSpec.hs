-- | Фильтры: TPT-однополюсник и лестничный фильтр.
module FilterSpec (tests) where

import Data.Vector.Unboxed qualified as U
import Sound.Sig.Core
import Sound.Sig.Filter
import Sound.Sig.Osc (saw, sine)
import Spectral (rms, spectrum)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Filter"
    [ onepoleTests
    , highpassTests
    , ladderLinear
    , ladderResonant
    , svfTests
    ]

svfTests :: TestTree
svfTests =
  testGroup
    "svf"
    [ -- Тождество топологии: три выхода складываются во вход при любых
      -- параметрах. Оно держит реализацию целиком - ошибка в любом из
      -- коэффициентов его ломает.
      testCase "hp + 2*R*bp + lp равно входу" $
        mapM_
          ( \(fc, res) ->
              let r = max 1e-4 (1 - res)
                  x = render defaultEnv (impulse 1)
                  o f = render defaultEnv (f (constant fc) (constant res) (impulse 1))
                  sum3 = U.zipWith3 (\h b l -> h + 2 * r * b + l) (o svfHigh) (o svfBand) (o svf)
               in assertBool
                    (show (fc, res, U.maximum (U.map abs (U.zipWith (-) sum3 x))))
                    (U.maximum (U.map abs (U.zipWith (-) sum3 x)) < 1e-12)
          )
          [(200, 0), (1000, 0.5), (5000, 0.9), (100, 1)]
    , -- Отклик прибит к аналитике, как у onepole и ladder. Тождество выше
      -- этого не даёт: оно выполняется при любых g и R, поэтому пропустило бы
      -- самую частую ошибку в SVF - забытое предыскажение (g = pi*fc/rate
      -- вместо tan). Высокий катофф обязателен: на 1 кГц tan даёт разницу в
      -- 0.14 процента, а на 8 кГц уже заметную.
      testCase "совпадает с аналитическим SVF" $
        mapM_
          ( \(fc, res) -> do
              let check name fx =
                    let db = worstDb (svfMag fc res name) (0.45 * rate) (responseOf 1 (fx (constant fc) (constant res)))
                     in assertBool (name <> " " <> show (fc, res) <> ": " <> show db <> " dB") (db < 0.1)
              check "lp" svf
              check "bp" svfBand
              check "hp" svfHigh
          )
          [(200, 0.293), (8000, 0.293), (8000, 0.9)]
    , -- Отображение резонанса в Q задокументировано числами, значит должно и
      -- проверяться: на катоффе усиление ФНЧ ровно Q = 1/(2R).
      testCase "резонанс отображается в Q" $
        mapM_
          ( \res ->
              let got = responseOf 1 (svf 1000 (constant res)) !! binOf 1000
                  want = 1 / (2 * max 1e-4 (1 - res))
               in assertBool (show (res, got, want)) (abs (got / want - 1) < 0.02)
          )
          [0, 0.293, 0.7, 0.9]
    , -- На единичном резонансе Q около 5000, то есть постоянная затухания
      -- 1.6 с: на окне в 0.2 с звон от нарастания не отличить, нужна секунда.
      testCase "на резонансе 1 звенит, но затухает" $ do
        let n = round rate
            xs = render defaultEnv (svf 1000 1 (fromSamples (1 : replicate (n - 1) 0)))
            half = U.length xs `div` 2
            early = rms (U.slice 0 half xs)
            late = rms (U.slice half half xs)
        assertBool (show (early, late)) (late < early && late > 0.1 * early)
        assertBool "NaN" (U.all (not . isNaN) xs)
    , -- Двенадцать децибел на октаву, в отличие от двадцати четырёх у ladder.
      testCase "спад около 12 дБ на октаву" $ do
        let mags = responseOf 1 (svf 100 0.293)
            magAt f = mags !! binOf f
            octave = 20 * logBase 10 (magAt 1000 / magAt 500)
        assertBool (show octave <> " dB") (abs (octave + 12) < 1)
    , testCase "на постоянном токе усиление единичное" $ do
        let mags = responseOf 1 (svf 1000 0.293)
        assertBool (show (mags !! 1)) (abs (mags !! 1 - 1) < 1e-3)
    , -- Верхний выход зеркален нижнему, полосовой имеет пик на катоффе.
      testCase "верхний и полосовой на своих местах" $ do
        let hp = responseOf 1 (svfHigh 1000 0.293)
            bp = responseOf 1 (svfBand 1000 0.293)
            at ms f = ms !! binOf f
        assertBool (show (at hp 100, at hp 10000)) (at hp 100 < 0.02 && at hp 10000 > 0.9)
        assertBool (show (at bp 1000, at bp 100, at bp 10000)) $
          at bp 1000 > 3 * at bp 100 && at bp 1000 > 3 * at bp 10000
    , testCase "режекторный проваливается на катоффе" $ do
        let ms = responseOf 1 (svfNotch 1000 0.293)
            at f = ms !! binOf f
        assertBool (show (at 1000, at 100)) (at 1000 < 0.05 * at 100)
    , -- Резонанс поднимает пик и не разносит фильтр даже на единице.
      testCase "резонанс поднимает пик и остаётся устойчивым" $ do
        let peakAt res = maximum (responseOf 1 (svf 1000 (constant res)))
            hot = render defaultEnv (svf 1000 1 (impulse 1))
        assertBool (show (peakAt 0, peakAt 0.9)) (peakAt 0.9 > 4 * peakAt 0)
        assertBool "разошёлся" (U.all (\v -> not (isNaN v) && abs v < 1e3) hot)
    , testCase "вырожденные параметры не дают NaN" $ do
        let run fc res = render defaultEnv (svf (constant fc) (constant res) (impulse 1))
            finite = U.all (\v -> not (isNaN v) && not (isInfinite v))
        assertBool "нулевой катофф" (finite (run 0 0.5))
        assertBool "катофф за Найквистом" (finite (run 1e9 0.5))
        assertBool "NaN в катоффе" (finite (run (0 / 0) 0.5))
        assertBool "NaN в резонансе" (finite (run 1000 (0 / 0)))
        assertBool "резонанс за диапазоном" (finite (run 1000 5))
    , testCase "не зависит от размера блока" $ do
        let big = render defaultEnv (svf 1000 0.7 (impulse 1))
            small = render defaultEnv {envBlock = 64} (svf 1000 0.7 (impulse 1))
        assertBool "расходится" (U.maximum (U.map abs (U.zipWith (-) big small)) < 1e-15)
    ]

rate :: Double
rate = envRate defaultEnv

sq :: Double -> Double
sq x = x * x

-- | Импульс заданной амплитуды длиной irLen.
impulse :: Double -> Sig
impulse amp = fromSamples (amp : replicate (irLen - 1) 0)

-- | 9600 сэмплов при 48 кГц дают бин ровно в 5 Гц, поэтому катофф 1000 Гц
-- попадает точно в бин 200 и пик резонанса не размазывается между бинами.
irLen :: Int
irLen = 9600

binHz :: Int -> Double
binHz k = fromIntegral k * rate / fromIntegral irLen

binOf :: Double -> Int
binOf f = round (f * fromIntegral irLen / rate)

-- | АЧХ по импульсной характеристике. Для линейного фильтра это точный
-- отклик: ни окна, ни усреднения, ни дисперсии оценки.
responseOf :: Double -> Fx -> [Double]
responseOf amp fx = map (/ amp) (spectrum (render defaultEnv (fx (impulse amp))))

-- | Точный отклик TPT-однополюсника. Билинейное преобразование предыскажает
-- частоту, поэтому в формуле tan, а не сама частота.
onepoleMag :: Double -> Double -> Double
onepoleMag fc f = 1 / sqrt (1 + sq (tan (pi * f / rate) / tan (pi * fc / rate)))

-- | Точный отклик TPT-SVF. Частоты предыскажены так же, как в реализации,
-- поэтому совпадение обязано быть с точностью до ошибки Double.
svfMag :: Double -> Double -> String -> Double -> Double
svfMag fc res kind f = case kind of
  "lp" -> base
  "bp" -> w * base
  _ -> w * w * base
  where
    r = max 1e-4 (1 - res)
    w = tan (pi * f / rate) / tan (pi * fc / rate)
    base = 1 / sqrt (sq (1 - w * w) + sq (2 * r * w))

-- | Аналоговый прототип из разд. 12: верен только там, где предыскажением
-- можно пренебречь.
analogLadderMag :: Double -> Double -> Double
analogLadderMag fc f = 1 / sq (1 + sq (f / fc))

-- | Худшее отклонение измеренного отклика от эталона, дБ, в полосе до
-- предельной частоты.
worstDb :: (Double -> Double) -> Double -> [Double] -> Double
worstDb want upTo mags =
  maximum
    [ abs (20 * logBase 10 (m / want f))
    | (k, m) <- zip [1 :: Int ..] (drop 1 mags)
    , let f = binHz k
    , f <= upTo
    ]

-- Однополюсник ------------------------------------------------------------

onepoleTests :: TestTree
onepoleTests =
  testGroup
    "onepole"
    [ testCase "на постоянном токе пропускает без изменений" $
        case responseOf 1 (onepole 1000) of
          dc : _ -> assertBool (show dc) (abs (dc - 1) < 1e-9)
          [] -> assertFailure "пустой спектр"
    , testCase "на Найквисте не пропускает" $ do
        let top = last (responseOf 1 (onepole 1000))
        assertBool (show top) (top < 1e-9)
    , testCase "совпадает с аналитическим однополюсником" $ do
        let db = worstDb (onepoleMag 1000) (0.45 * rate) (responseOf 1 (onepole 1000))
        assertBool (show db <> " dB") (db < 0.05)
    , testCase "работает и на низком катоффе" $ do
        let db = worstDb (onepoleMag 100) (0.45 * rate) (responseOf 1 (onepole 100))
        assertBool (show db <> " dB") (db < 0.05)
    , testCase "длина как у входа" $ do
        U.length (render defaultEnv (onepole 1000 (impulse 1))) @?= irLen
    , -- Состояние обязано переезжать через границу блока без следа.
      testCase "не зависит от размера блока" $ do
        let big = render defaultEnv (onepole 1000 (impulse 1))
            small = render defaultEnv {envBlock = 64} (onepole 1000 (impulse 1))
        assertBool "расходится" (U.maximum (U.map abs (U.zipWith (-) big small)) < 1e-15)
    ]

-- Фильтр верхних частот ----------------------------------------------------

-- | Точный отклик ФВЧ первого порядка: дополнение однополюсника до единицы.
highpassMag :: Double -> Double -> Double
highpassMag fc f = t / sqrt (sq (tan (pi * fc / rate)) + sq t)
  where
    t = tan (pi * f / rate)

highpassTests :: TestTree
highpassTests =
  testGroup
    "highpass"
    [ testCase "постоянный ток не проходит" $
        case responseOf 1 (highpass 1000) of
          dc : _ -> assertBool (show dc) (dc < 1e-12)
          [] -> assertFailure "пустой спектр"
    , testCase "совпадает с аналитическим ФВЧ" $ do
        let db = worstDb (highpassMag 1000) (0.45 * rate) (responseOf 1 (highpass 1000))
        assertBool (show db <> " dB") (db < 0.05)
    , -- Ради этого он и нужен: убрать DC, не тронув звук.
      testCase "срез в единицы герц не трогает звук" $ do
        let db = worstDb (highpassMag 5) (0.45 * rate) (responseOf 1 (highpass 5))
        assertBool (show db <> " dB") (db < 0.05)
        let at100 = 20 * logBase 10 (highpassMag 5 100)
        assertBool (show at100) (abs at100 < 0.02)
    ]

-- Лестничный фильтр без резонанса -----------------------------------------

ladderLinear :: TestTree
ladderLinear =
  testGroup
    "ladder без резонанса"
    [ -- Критерий разд. 11: при res=0 это ровно четыре однополюсника.
      testCase "равен четырём однополюсникам" $ do
        let want f = onepoleMag 1000 f ** 4
            db = worstDb want (0.45 * rate) (responseOf 1 (ladder 1000 0))
        assertBool (show db <> " dB") (db < 0.05)
    , testCase "и на низком катоффе" $ do
        let want f = onepoleMag 200 f ** 4
            db = worstDb want (0.45 * rate) (responseOf 1 (ladder 200 0))
        assertBool (show db <> " dB") (db < 0.05)
    , -- Аналоговая формула разд. 12 совпадает только внизу: билинейное
      -- преобразование предыскажает частоту, и уже к 0.05*sr расхождение
      -- упирается в 0.2 дБ.
      testCase "аналоговый прототип верен на низких частотах" $ do
        let db = worstDb (analogLadderMag 1000) (0.04 * rate) (responseOf 1 (ladder 1000 0))
        assertBool (show db <> " dB") (db < 0.2)
    , testCase "аналоговый прототип расходится у Найквиста" $ do
        let db = worstDb (analogLadderMag 1000) (0.45 * rate) (responseOf 1 (ladder 1000 0))
        assertBool (show db <> " dB") (db > 10)
    , -- Спад 24 дБ на октаву это асимптотика, точно она не достигается
      -- нигде: ближе к катоффу мешает слагаемое 1 в знаменателе, дальше
      -- от него предыскажение (на 8-16 кГц отношение частот 3, а не 2, и
      -- спад выходит 38 дБ). Сам отклик уже прибит поточечно тестом выше,
      -- здесь проверяется только характер.
      testCase "спад около 24 дБ на октаву внизу" $ do
        let mags = responseOf 1 (ladder 100 0)
            magAt f = mags !! binOf f
            octave = 20 * logBase 10 (magAt 1000 / magAt 500)
        assertBool (show octave <> " dB") (abs (octave + 24) < 1)
    , testCase "не зависит от размера блока" $ do
        let big = render defaultEnv (ladder 1000 0 (impulse 1))
            small = render defaultEnv {envBlock = 64} (ladder 1000 0 (impulse 1))
        assertBool "расходится" (U.maximum (U.map abs (U.zipWith (-) big small)) < 1e-15)
    ]

-- Резонанс ----------------------------------------------------------------

-- | Точное решение линейной петли. Цепочка аффинна по u, поэтому
-- @u = (x - k*c)/(1 + k*G^4)@, где c это её выход при u=0. Эталон годится,
-- пока амплитуда мала и tanh почти линеен.
exactLinearLoop :: Double -> Double -> U.Vector Double -> U.Vector Double
exactLinearLoop fc res xs = U.fromList (go (0, 0, 0, 0) (U.toList xs))
  where
    g = tan (pi * fc / rate)
    bigG = g / (1 + g)
    k = 4 * res
    onePole s x = let v = (x - s) * bigG; y = v + s in (y, y + v)
    four (s1, s2, s3, s4) u =
      let (y1, t1) = onePole s1 u
          (y2, t2) = onePole s2 y1
          (y3, t3) = onePole s3 y2
          (y4, t4) = onePole s4 y3
       in (y4, (t1, t2, t3, t4))
    go _ [] = []
    go ss (x : rest) =
      let (c, _) = four ss 0
          u = (x - k * c) / (1 + k * bigG ** 4)
          (y, ss') = four ss u
       in y : go ss' rest

-- | Относительное расхождение фильтра с точным решением линейной петли.
loopError :: Double -> Double -> Double
loopError fc res = U.maximum (U.map abs (U.zipWith (-) got want)) / U.maximum (U.map abs want)
  where
    xs = render defaultEnv (takeSec 0.02 (saw 200 * 1e-3))
    got = render defaultEnv (ladder (constant fc) (constant res) (fromSamples (U.toList xs)))
    want = exactLinearLoop fc res xs

-- | Отклик на удар с последующей тишиной длиной dur секунд.
ring :: Double -> Double -> Sig
ring res dur = ladder 1000 (constant res) (fromSamples (1 : replicate (n - 1) 0))
  where
    n = round (dur * rate)

-- | Среднеквадратичное в окне [from, from + len) секунд.
windowRms :: Double -> Double -> U.Vector Double -> Double
windowRms from len xs = rms (U.slice (round (from * rate)) (round (len * rate)) xs)

ladderResonant :: TestTree
ladderResonant =
  testGroup
    "ladder с резонансом"
    [ -- На катоффе четыре звена дают ровно -0.25 (модуль 1/4, фаза 180),
      -- поэтому замкнутый контур усиливает в 1/(1-res) раз. Малая амплитуда
      -- держит tanh в линейной области, так что это точная проверка петли.
      testCase "усиление на катоффе равно 0.25/(1-res)" $ do
        let gain res = responseOf 1e-3 (ladder 1000 (constant res)) !! binOf 1000
            check res = do
              let want = 0.25 / (1 - res)
                  got = gain res
              assertBool
                ("res=" <> show res <> ": ждали " <> show want <> ", получили " <> show got)
                (abs (got - want) < 0.03 * want)
        mapM_ check [0, 0.5, 0.9]
    , testCase "резонанс поднимает пик у катоффа" $ do
        let peak res =
              maximum
                [ m
                | (k, m) <- zip [0 :: Int ..] (responseOf 1e-3 (ladder 1000 (constant res)))
                , binHz k > 600
                , binHz k < 1600
                ]
        assertBool "нет подъёма" (peak 0.9 > 4 * peak 0)
    , -- Критерий разд. 11: самовозбуждается и не расходится.
      testCase "при res=0.99 звенит долго" $ do
        let xs = render defaultEnv (ring 0.99 0.3)
        assertBool "затухло" (windowRms 0.1 0.1 xs > 1e-3)
        assertBool "разошлось" (U.maximum (U.map abs xs) < 10)
    , testCase "при res=0 не звенит" $ do
        let xs = render defaultEnv (ring 0 0.3)
        assertBool "звенит без резонанса" (windowRms 0.1 0.1 xs < 1e-9)
    , testCase "звенит на частоте катоффа" $ do
        let xs = render defaultEnv (ring 0.99 0.3)
            win = U.toList (U.slice (round (0.1 * rate)) (round (0.1 * rate)) xs)
            ups = length (filter id (zipWith (\a b -> a < 0 && b >= 0) win (drop 1 win)))
            hz = fromIntegral ups / 0.1 :: Double
        assertBool (show hz <> " Гц") (abs (hz - 1000) < 60)
    , testCase "при res=1 остаётся ограниченным" $ do
        let xs = render defaultEnv (ring 1 0.5)
        assertBool "NaN" (U.all (\v -> not (isNaN v) && not (isInfinite v)) xs)
        assertBool "разошлось" (U.maximum (U.map abs xs) < 10)
    , -- Нелинейность в петле обязана ограничивать амплитуду, а не резать
      -- её в клипинг.
      testCase "громкий вход при резонансе не расходится" $ do
        let loud = fromSamples (replicate (round (0.2 * rate)) 1)
            xs = render defaultEnv (ladder 1000 0.99 loud)
        assertBool "NaN" (U.all (\v -> not (isNaN v) && not (isInfinite v)) xs)
        assertBool "разошлось" (U.maximum (U.map abs xs) < 20)
    , -- Без tanh петля на res=1 линейна и её усиление на катоффе ровно 1,
      -- поэтому синус на этой же частоте раскачивал бы амплитуду линейно
      -- по времени. Ограничивает её именно нелинейность.
      testCase "tanh в петле ограничивает раскачку на резонансе" $ do
        let drive = takeSec 0.5 (sine 1000 * 0.5)
            ys = render defaultEnv (ladder 1000 1 drive)
            peak = U.maximum (U.map abs ys)
        assertBool ("раскачалось до " <> show peak) (peak < 5)
        -- Возбуждение 0.5, то есть резонанс всё же усиливает, просто петля
        -- сжата нелинейностью.
        assertBool ("не резонирует, пик " <> show peak) (peak > 0.7)
    , -- При res=0 поле yPrev в состоянии не влияет ни на что, поэтому
      -- перенос состояния между блоками надо проверять и с резонансом.
      testCase "не зависит от размера блока и при резонансе" $ do
        let input = takeSec 0.05 (sine 300 * 0.5)
            big = render defaultEnv (ladder 1000 0.9 input)
            small = render defaultEnv {envBlock = 64} (ladder 1000 0.9 input)
        assertBool "расходится" (U.maximum (U.map abs (U.zipWith (-) big small)) < 1e-15)
    , -- У высокого катоффа k*G^4 подходит к единице и итерация перестаёт
      -- сходиться. Ограниченность и повторяемость обязаны сохраниться.
      testCase "высокий катофф с резонансом остаётся ограниченным" $ do
        let input = takeSec 0.05 (saw 200 * 0.5)
            cut = constant (0.35 * rate)
            big = render defaultEnv (ladder cut 0.95 input)
            small = render defaultEnv {envBlock = 64} (ladder cut 0.95 input)
        assertBool "NaN" (U.all (\v -> not (isNaN v) && not (isInfinite v)) big)
        assertBool "разошлось" (U.maximum (U.map abs big) < 20)
        assertBool "зависит от блока" (U.maximum (U.map abs (U.zipWith (-) big small)) < 1e-15)
    , -- Четыре итерации из разд. 6.3 сходятся, пока множитель сжатия
      -- k*G^4 мал. На рабочих катоффах (патч разд. 9 живёт ниже 0.12*sr, а
      -- под oversample и того ниже) он около 0.014, и решение практически
      -- точное.
      testCase "итерации сходятся к точному решению петли" $ do
        let err = loopError (0.1 * rate) 0.95
        assertBool ("относительная ошибка " <> show err) (err < 1e-6)
    , -- Граница метода: к 0.35*sr множитель сжатия доходит до 0.73, четырёх
      -- итераций не хватает, и фильтр самовозбуждается там, где точное
      -- решение петли ещё устойчиво. Факт зафиксирован, чтобы не считать
      -- поведение у Найквиста достоверным.
      testCase "у высокого катоффа четырёх итераций не хватает" $ do
        let err = loopError (0.35 * rate) 0.95
        assertBool ("относительная ошибка " <> show err) (err > 0.1)
    , -- NaN в управляющем сигнале не должен протекать на выход: катофф
      -- падает на нижнюю границу, резонанс на ноль.
      testCase "NaN в управлении не протекает на выход" $ do
        let n = round (0.01 * rate)
            input = fromSamples (replicate n 0.5)
            byCut = render defaultEnv (ladder (constant (0 / 0)) 0.5 input)
            byRes = render defaultEnv (ladder 1000 (constant (0 / 0)) input)
        assertBool "NaN от катоффа" (U.all (not . isNaN) byCut)
        assertBool "NaN от резонанса" (U.all (not . isNaN) byRes)
    , testCase "модуляция катоффа не ломает фильтр" $ do
        let n = round (0.2 * rate)
            sweepCut = fromSamples [50 + 20000 * fromIntegral i / fromIntegral n | i <- [0 .. n - 1]]
            xs = render defaultEnv (ladder sweepCut 0.9 (fromSamples (replicate n 0.5)))
        assertBool "NaN" (U.all (\v -> not (isNaN v) && not (isInfinite v)) xs)
        assertBool "разошлось" (U.maximum (U.map abs xs) < 10)
    ]
