-- | Бинауральная панорама на измеренных откликах (этап M10).
--
-- Набор приезжает через flake, путь берётся из HSIG_HRTF. Загрузка идёт один
-- раз на всю группу: 37 файлов с пересчётом частоты стоят заметно дороже
-- самих проверок.
module HrtfSpec (tests) where

import Data.Vector.Unboxed qualified as U
import Sound.Sig
import Sound.Sig.HRTF
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = withResource (loadHrtfEnv defaultEnv) (const (pure ())) hrtfTests

rate :: Double
rate = envRate defaultEnv

-- | Модуль спектра короткого отклика на частоте: ДПФ в одной точке.
magAt :: Double -> U.Vector Double -> Double
magAt f ir = sqrt (re * re + im * im)
  where
    w k = 2 * pi * f * fromIntegral k / rate
    re = U.sum (U.imap (\k v -> v * cos (w k)) ir)
    im = U.sum (U.imap (\k v -> v * sin (w k)) ir)

db :: Double -> Double
db v = 20 * logBase 10 (max 1e-9 v)

deg :: Double -> Double
deg d = d * pi / 180

hrtfTests :: IO Hrtf -> TestTree
hrtfTests get =
  testGroup
    "HRTF"
    [ testGroup
        "набор"
        [ -- Горизонтальная плоскость измерена с шагом пять градусов; всего
          -- направлений больше, потому что загружается вся полусфера.
          testCase "горизонт с шагом пять градусов" $ do
            h <- get
            let itdAt d = uncurry (-) (delayAt h (deg d) 0)
            -- Между узлами сетки значения ровно посередине: шаг совпал.
            assertBool "шаг не пять градусов" (abs (itdAt 2.5 - (itdAt 0 + itdAt 5) / 2) < 1e-9)
        , testCase "отклики пересчитаны на частоту рендера" $ do
            h <- get
            hrtfRate h @?= rate
            -- 128 отводов на 44.1 кГц становятся примерно 139 на 48, минус
            -- снятая задержка.
            let (l, _) = dirAt h 0 0
            assertBool (show (U.length l)) (U.length l > 100 && U.length l < 145)
        ]
    , testGroup
        "межушная разница"
        [ -- Спереди и сзади уши равноудалены, сбоку разница максимальна.
          testCase "спереди и сзади разницы нет" $ do
            h <- get
            let itdAt a = uncurry (-) (delayAt h a 0)
            itdAt 0 @?= 0
            itdAt pi @?= 0
        , testCase "справа задерживается левое ухо" $ do
            h <- get
            let (l, r) = delayAt h (deg 90) 0
            assertBool (show (l, r)) (l > r)
        , testCase "слева наоборот" $ do
            h <- get
            let (l, r) = delayAt h (deg 270) 0
            assertBool (show (l, r)) (r > l)
        , -- Физический потолок: путь вокруг головы это около 0.7 мс.
          testCase "разница не выходит за 750 микросекунд" $ do
            h <- get
            let itds = [abs (uncurry (-) (delayAt h (deg d) 0)) / rate | d <- [0, 5 .. 355]]
            assertBool (show (maximum itds * 1e6)) (maximum itds < 750e-6)
        , testCase "к боку разница растёт монотонно" $ do
            h <- get
            let itds = [uncurry (-) (delayAt h (deg d) 0) | d <- [0, 15 .. 90]]
            assertBool (show itds) (and (zipWith (<=) itds (drop 1 itds)))
        , -- Между измерениями сетки задержка идёт плавно: на этом держится
          -- отсутствие щелчков у движущегося источника.
          testCase "между измерениями разница интерполируется" $ do
            h <- get
            let a = fst (delayAt h (deg 0) 0)
                b = fst (delayAt h (deg 5) 0)
                mid = fst (delayAt h (deg 2.5) 0)
            assertBool (show (a, mid, b)) (abs (mid - (a + b) / 2) < 1e-9)
        ]
    , testGroup
        "тень головы"
        [ testCase "дальнее ухо тише ближнего" $ do
            h <- get
            let (l, r) = dirAt h (deg 90) 0
                loud = U.maximum . U.map abs
            assertBool (show (loud l, loud r)) (db (loud r) - db (loud l) > 10)
        , testCase "спереди уши равны" $ do
            h <- get
            let (l, r) = dirAt h 0 0
            assertBool "перед несимметричен" (U.sum (U.map abs (U.zipWith (-) l r)) < 1e-12)
        ]
    , testGroup
        "перед и зад"
        [ -- То, ради чего этап и делался: спереди ушная раковина даёт провал
          -- около 8 кГц, сзади его нет. Параметрическая модель так не умеет,
          -- и это проверяется прямо здесь.
          testCase "провал раковины есть спереди и нет сзади" $ do
            h <- get
            let front = fst (dirAt h 0 0)
                back = fst (dirAt h pi 0)
            assertBool "нет провала на 8 кГц" (db (magAt 8000 front) < db (magAt 8000 back) - 4)
        , testCase "выше провала картина обратная" $ do
            h <- get
            let front = fst (dirAt h 0 0)
                back = fst (dirAt h pi 0)
            assertBool "нет подъёма на 14 кГц" (db (magAt 14000 front) > db (magAt 14000 back) + 6)
        , -- Контраст с параметрической моделью: она видит только синус угла,
          -- поэтому перед и зад у неё почти совпадают. Почти - потому что
          -- sin pi в double не ровно ноль.
          testCase "orbit не отличает перед от зада" $ do
            let src = takeSec 0.05 (share (saw 300 * 0.5))
                Stereo fl _ = orbit 0 src
                Stereo bl _ = orbit (constant pi) src
                power v = U.sum (U.map (\x -> x * x) v)
                f = render defaultEnv fl
                b = render defaultEnv bl
                diff = 10 * logBase 10 (power (U.zipWith (-) f b) / power f)
            assertBool (show diff) (diff < -100)
        , -- А у измеренных откликов разница между передом и задом это
          -- заметная доля энергии: именно она и слышна как направление.
          testCase "у HRTF перед и зад различаются" $ do
            h <- get
            let src = takeSec 0.05 (share (saw 300 * 0.5))
                Stereo fl _ = binaural h 0 0 src
                Stereo bl _ = binaural h (constant pi) 0 src
                power v = U.sum (U.map (\x -> x * x) v)
                f = render defaultEnv fl
                b = render defaultEnv bl
                diff = 10 * logBase 10 (power (U.zipWith (-) f b) / power f)
            assertBool (show diff) (diff > -12)
        ]
    , testGroup
        "вертикаль"
        [ testCase "полусфера от минус сорока до зенита" $ do
            h <- get
            planeCount h @?= 14
            dirCount h @?= 710
            elevations h @?= [-40, -30 .. 90]
        , -- Классический закон: межушная разница идёт как косинус элевации и
          -- в зените обращается в ноль. Это первый признак того, что
          -- вертикаль загружена правильно, а не перепутана с азимутом.
          testCase "к зениту межушная разница падает до нуля" $ do
            h <- get
            let itds = [uncurry (-) (delayAt h (deg 90) (deg e)) | e <- [0, 20 .. 90]]
            assertBool (show itds) (and (zipWith (>) itds (drop 1 itds)))
            assertBool (show (last itds)) (abs (last itds) < 1e-9)
        , -- Признак высоты это спектр: он меняется десятками децибел там, где
          -- работает раковина.
          testCase "спектр спереди зависит от высоты" $ do
            h <- get
            let flat = fst (dirAt h 0 0)
                up = fst (dirAt h 0 (deg 40))
            assertBool "высота не слышна" (abs (db (magAt 8000 up) - db (magAt 8000 flat)) > 10)
        , testCase "за краями сетки элевация зажимается" $ do
            h <- get
            let low = fst (dirAt h 0 (deg (-80)))
                edge = fst (dirAt h 0 (deg (-40)))
            low @?= edge
        , -- В зените измерение одно, поэтому азимут там ничего не меняет.
          testCase "в зените азимут не важен" $ do
            h <- get
            fst (dirAt h 0 (deg 90)) @?= fst (dirAt h (deg 123) (deg 90))
        , testCase "движение по вертикали не даёт щелчков" $ do
            h <- get
            let el = constant (deg 60) * line [(0, 0), (1, 1)]
                src = takeSec 1 (share (saw 220 * 0.4))
                Stereo l _ = binaural h 0 el src
                xs = render defaultEnv l
                jumps = U.maximum (U.map abs (U.zipWith (-) (U.drop 1 xs) xs))
                still = render defaultEnv (takeSec 1 (saw 220 * 0.4))
                ref = U.maximum (U.map abs (U.zipWith (-) (U.drop 1 still) still))
            assertBool (show (jumps, ref)) (jumps < 2 * ref)
        ]
    , testGroup
        "рендер"
        [ testCase "длина сохраняется" $ do
            h <- get
            let src = takeSec 0.1 (saw 220 * 0.4)
                Stereo l r = binaural h 0 0 src
            U.length (render defaultEnv l) @?= round (0.1 * rate)
            U.length (render defaultEnv r) @?= round (0.1 * rate)
        , testCase "тишина остаётся тишиной" $ do
            h <- get
            let Stereo l _ = binaural h 0 0 (takeSec 0.05 (constant 0))
            U.all (== 0) (render defaultEnv l) @?= True
        , -- Движущийся источник не должен щёлкать: скачок между соседними
          -- сэмплами ограничен, иначе смена отклика слышна как треск.
          testCase "движение не даёт щелчков" $ do
            h <- get
            let angle = 2 * pi * line [(0, 0), (1, 1)]
                src = takeSec 1 (share (saw 220 * 0.4))
                Stereo l _ = binaural h angle 0 src
                xs = render defaultEnv l
                jumps = U.maximum (U.map abs (U.zipWith (-) (U.drop 1 xs) xs))
                still = render defaultEnv (takeSec 1 (saw 220 * 0.4))
                ref = U.maximum (U.map abs (U.zipWith (-) (U.drop 1 still) still))
            assertBool (show (jumps, ref)) (jumps < 2 * ref)
        , testCase "рендер детерминирован" $ do
            h <- get
            let src = takeSec 0.1 (saw 220 * 0.4)
                Stereo l _ = binaural h (constant 1) 0 src
            render defaultEnv l @?= render defaultEnv l
        , -- Источник справа громче в правом ухе, слева - в левом.
          testCase "образ идёт за углом" $ do
            h <- get
            let src = takeSec 0.2 (share (saw 220 * 0.4))
                power v = U.sum (U.map (\x -> x * x) (render defaultEnv v))
                Stereo rl rr = binaural h (constant (deg 90)) 0 src
                Stereo ll lr = binaural h (constant (deg 270)) 0 src
            assertBool "справа не громче справа" (power rr > 2 * power rl)
            assertBool "слева не громче слева" (power ll > 2 * power lr)
        ]
    ]
