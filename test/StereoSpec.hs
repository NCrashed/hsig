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

-- | Межушная разница в секундах, положительная когда правое ухо раньше.
--
-- Меряем по разности фаз низкочастотного тона: именно эта задержка и
-- работает как признак локализации ниже полутора килогерц. Взаимная
-- корреляция на шуме тут не годится - она идёт за пиком импульсной
-- характеристики и не видит групповой задержки теневого фильтра, то есть
-- показала бы верное число даже у модели, где фильтр эту задержку не
-- скомпенсировал. Период тона 5 мс против 0.65 мс разницы, так что
-- неоднозначности нет.
itdSec :: Double -> Double
-- atan2 (Im, Re) от sin (w n + p) даёт pi/2 - p, то есть растёт с
-- задержкой; поэтому левое ухо минус правое, а не наоборот.
itdSec a = (phaseOf xs - phaseOf ys) / (2 * pi * f)
  where
    f = 200
    Stereo l r = orbit (constant a) (sine (constant f))
    xs = grab l
    ys = grab r
    grab s = U.drop (round (0.1 * envRate defaultEnv)) (render defaultEnv (takeSec 0.3 s))
    phaseOf v = atan2 (part sin v) (part cos v)
    part g v = U.sum (U.imap (\i x -> x * g (2 * pi * f * fromIntegral i / envRate defaultEnv)) v)

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
    , -- Главный признак обхода: ближнее ухо слышит раньше, и ровно на
      -- заявленные 0.65 мс. Без вычитания групповой задержки теневого
      -- фильтра тут вышло бы 0.75 мс.
      testCase "межушная разница по краям это 0.65 мс" $ do
        let right = itdSec (pi / 2)
            left = itdSec (negate pi / 2)
            front = itdSec 0
        assertBool (show right) (abs (right - 0.00065) < 3e-5)
        assertBool (show left) (abs (left + 0.00065) < 3e-5)
        assertBool (show front) (abs front < 1e-6)
    , -- Признака, различающего перед и зад, в модели нет, и это надо знать:
      -- полный оборот читается как обход через бока. Лечится HRTF.
      testCase "перед и зад неразличимы" $ do
        let front = render defaultEnv (takeSec 0.05 (leftChan (still 0 (noise 0))))
            back = render defaultEnv (takeSec 0.05 (leftChan (still pi (noise 0))))
        assertBool "модель различает" (U.maximum (U.map abs (U.zipWith (-) front back)) < 1e-12)
    , -- Тень головы: у дальнего уха заваливается верх. Абсолютной энергии
      -- мало, разница уровней даёт то же отношение и сама по себе: убери
      -- фильтр совсем, и критерий "впятеро" всё равно выполнится. Поэтому
      -- рядом стоит наклон, то есть доля верха в собственной энергии канала,
      -- и вот он без фильтра равен единице.
      testCase "дальнее ухо темнее ближнего" $ do
        let Stereo l r = still (pi / 2) (saw 300)
            top s = energy 0.2 (highpass 4000 s)
            tilt s = top s / energy 0.2 s
        assertBool (show (top l, top r)) (top l < 0.2 * top r)
        assertBool (show (tilt l, tilt r)) (tilt l < 0.5 * tilt r)
    , -- Разница уровней по краям заявлена в 9.8 дБ.
      testCase "разница уровней по краям" $ do
        let Stereo l r = still (pi / 2) (saw 300)
            ild = 10 * logBase 10 (energy 0.2 r / energy 0.2 l)
        assertBool (show ild) (ild > 9 && ild < 12)
    , -- Закон равной мощности держит сумму квадратов усилений тождественно
      -- единицей, поэтому по кругу меняется только потеря в теневом фильтре,
      -- а это 1.8 процента. Порог в двойку пропустил бы и линейный закон
      -- панорамы с его провалом в центре, и жёсткую панораму.
      testCase "по кругу энергия не проваливается" $ do
        let total a = let Stereo l r = still a (saw 300) in energy 0.2 l + energy 0.2 r
            angles = [0, pi / 4, pi / 2, 3 * pi / 4, pi, 5 * pi / 4, 3 * pi / 2]
            es = map total angles
        assertBool (show es) (maximum es < 1.1 * minimum es)
    , -- Хвост дальнего канала отстаёт на 0.85 мс, поэтому ноте нужны те же
      -- 0.85 мс тишины после конца. Без них канал обрывается ступенькой, и
      -- рецепт в хаддоке обязан её убирать.
      testCase "добивка спасает хвост дальнего канала" $ do
        -- Источник без колебания: у тона последний сэмпл попадает в
        -- случайную точку периода и ступеньку не показывает.
        let dur = 0.05
            note = line [(0, 1), (dur, 0)]
            lastOf s = U.last (render defaultEnv s)
            -- Источник слева, значит дальнее ухо правое.
            far s = let Stereo _ r = still (negate pi / 2) s in r
            bare = abs (lastOf (far note))
            padded = abs (lastOf (far (padSec (dur + 0.00085) note)))
        -- Ровного нуля у добитого не будет: теневой фильтр отстаёт от спада
        -- на свою постоянную времени. Важно, что ступенька уходит в разы.
        assertBool (show bare) (bare > 1e-3)
        assertBool (show (bare, padded)) (padded < 0.3 * bare)
    , -- Гребёнка при сведении в моно, про которую написано в хаддоке. Она
      -- глубже всего не по бокам, а у центра: там уровни ушей почти равны и
      -- пути гасят друг друга, а по бокам дальнее ухо задавлено тенью.
      testCase "в моно провал глубже у центра, чем по бокам" $ do
        -- Считать с самого начала нельзя: до прихода задержанного пути
        -- гашения нет, и этот всплеск на глубоком провале даёт половину
        -- энергии окна.
        let steady s = U.drop (round (0.1 * envRate defaultEnv)) (render defaultEnv (takeSec 0.3 s))
            power s = U.sum (U.map (\x -> x * x) (steady s))
            sum2 f a =
              let Stereo l r = still a (sine (constant f))
               in 10 * logBase 10 (power ((l + r) * constant 0.5) / power r)
            near = sum2 4400 (pi / 18)
            side = sum2 800 (pi / 2)
        assertBool (show near) (near < (-15))
        assertBool (show (near, side)) (side > near + 8)
    , -- Межушная разница обязана расти по синусу угла, а не скакать: на
      -- промежуточных углах образ иначе застревал бы или проскакивал.
      testCase "задержка растёт по синусу угла" $
        mapM_
          ( \a ->
              let want = 0.00065 * sin a
               in assertBool (show (a, itdSec a, want)) (abs (itdSec a - want) < 3e-5)
          )
          [pi / 6, pi / 4, pi / 3, pi / 2, 5 * pi / 6]
    , -- Длина по короткому из угла и источника, как у vdelay под ним.
      testCase "длина по короткому" $ do
        let Stereo l _ = orbit (takeSec 0.1 (phase 1)) (takeSec 0.3 (saw 300))
            Stereo l' _ = orbit (phase 1) (takeSec 0.2 (saw 300))
        U.length (render defaultEnv l) @?= round (0.1 * envRate defaultEnv)
        U.length (render defaultEnv l') @?= round (0.2 * envRate defaultEnv)
    , -- NaN в угле не должен разносить NaN по всему выходу.
      testCase "NaN в угле прижимается к краю" $ do
        let Stereo l r = still (0 / 0) (saw 300)
            finite s = U.all (\x -> not (isNaN x) && not (isInfinite x)) (render defaultEnv (takeSec 0.05 s))
        assertBool "левый" (finite l)
        assertBool "правый" (finite r)
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
