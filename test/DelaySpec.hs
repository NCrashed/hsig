-- | Задержки: линия, гребёнка, фазовый фильтр.
module DelaySpec (tests) where

import Data.Vector.Unboxed qualified as U
import Sound.Sig.Core
import Sound.Sig.Delay
import Sound.Sig.Osc (phase, sine)
import Spectral (spectrum)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Delay"
    [ delayTests
    , vdelayTests
    , combTests
    , allpassTests
    ]

vdelayTests :: TestTree
vdelayTests =
  testGroup
    "vdelay"
    [ -- Целое время сдвигает ровно на столько сэмплов и ничего не искажает.
      testCase "целая задержка это сдвиг" $ do
        let n = 200
            src = fromSamples [fromIntegral i / 100 | i <- [1 .. n :: Int]]
            xs = render defaultEnv (vdelay 0.01 (constant (10 / rate)) src)
            want = render defaultEnv src
        U.length xs @?= n
        assertBool "хвост разошёлся" $
          U.all (\i -> abs (xs U.! (i + 10) - want U.! i) < 1e-12) (U.enumFromN 0 (n - 10))
    , -- Длину сохраняет и режет по короткому: это модуляционная задержка.
      testCase "длина по короткому" $ do
        let xs = render defaultEnv (vdelay 0.01 (constant 0.001) (takeSec 0.05 (sine 200)))
            ys = render defaultEnv (vdelay 0.01 (takeSec 0.02 (constant 0.001)) (takeSec 0.05 (sine 200)))
        U.length xs @?= round (0.05 * rate)
        U.length ys @?= round (0.02 * rate)
    , -- Дробная задержка обязана попадать в аналитический сдвиг, а не просто
      -- быть где-то между соседями: сравнение с их средним пропустило бы
      -- линейную интерполяцию, у неё это среднее и есть.
      testCase "дробная задержка совпадает с аналитикой" $ do
        let f = 500
            d = 10.5
            w = 2 * pi * f / rate
            xs = render defaultEnv (takeSec 0.02 (vdelay 0.01 (constant (d / rate)) (sine (constant f))))
            err = maximum [abs (xs U.! i - sin (w * (fromIntegral i - d))) | i <- [100 .. 900]]
        -- Линейная промахнулась бы на 5e-4, кубическая на единицы 1e-7.
        assertBool (show err) (err < 1e-5)
    , -- Ради чего всё: на плавно едущем времени выход обязан быть гладким.
      -- Порог тут не запас, а граница: округление до сэмпла двигает
      -- указатель на два сэмпла за один выходной, то есть даёт ровно вдвое
      -- больший скачок, чем сам синус.
      testCase "движение времени не щёлкает" $ do
        let sweep = constant 0.0005 + constant 0.0005 * mapSig sin (phase 0.5)
            xs = render defaultEnv (takeSec 1 (vdelay 0.002 sweep (sine 300)))
            jumps = U.maximum (U.map abs (U.zipWith (-) (U.tail xs) xs))
            -- Синус 300 Гц сам меняется не быстрее 0.04 за сэмпл.
            smooth = U.maximum (U.map abs (U.zipWith (-) (U.tail plain) plain))
            plain = render defaultEnv (takeSec 1 (sine 300))
        assertBool (show (jumps, smooth)) (jumps < 1.5 * smooth)
        -- Иначе тождественный ноль прошёл бы с блеском.
        assertBool (show (U.maximum (U.map abs xs))) (U.maximum (U.map abs xs) > 0.9)
    , -- Разд. 12 требует побитового совпадения, а не близости: весь путь
      -- посэмплово рекуррентный и переносит состояние через границу блока
      -- без потерь.
      testCase "не зависит от размера блока" $ do
        let sig = vdelay 0.002 (constant 0.0007 + constant 0.0003 * mapSig sin (phase 3)) (sine 300)
            big = render defaultEnv (takeSec 0.3 sig)
            small = render defaultEnv {envBlock = 61} (takeSec 0.3 sig)
        U.length small @?= U.length big
        big @?= small
    , -- Зажим виден по величине задержки, а не по конечности: конечным
      -- останется и чтение мимо окна интерполяции. Рампа проходит через
      -- кубическую интерполяцию точно, поэтому задержку читаем прямо.
      testCase "время за пределами зажимается" $ do
        let n = 400
            src = fromSamples (map fromIntegral [0 .. n - 1 :: Int])
            delayAt t =
              let xs = render defaultEnv (vdelay 0.001 (constant t) src)
               in fromIntegral (n - 50) - xs U.! (n - 50)
        assertBool (show (delayAt (-1))) (abs (delayAt (-1) - 1) < 1e-9)
        assertBool (show (delayAt (0 / 0))) (abs (delayAt (0 / 0) - 1) < 1e-9)
        -- Потолок это размер буфера, он на пару сэмплов выше maxSec.
        let top = delayAt 10
        assertBool (show top) (top >= 48 && top <= 50)
    ]

rate :: Double
rate = envRate defaultEnv

-- | Импульс, за которым n нулей.
impulse :: Int -> Sig
impulse n = fromSamples (1 : replicate (n - 1) 0)

delayTests :: TestTree
delayTests =
  testGroup
    "delay"
    [ -- Задержка сдвигает сигнал, а не режет его, поэтому и длиннее ровно
      -- на сдвиг.
      testCase "сдвигает импульс на заданное время" $ do
        let xs = render defaultEnv (delay 0.01 (impulse 100))
        U.maxIndex xs @?= 480
        U.length xs @?= 100 + 480
    , testCase "нулевая задержка ничего не меняет" $ do
        render defaultEnv (delay 0 (impulse 10)) @?= render defaultEnv (impulse 10)
    , testCase "не зависит от размера блока" $ do
        let big = render defaultEnv (delay 0.01 (impulse 2000))
            small = render defaultEnv {envBlock = 64} (delay 0.01 (impulse 2000))
        big @?= small
    ]

combTests :: TestTree
combTests =
  testGroup
    "comb"
    [ -- Импульсная характеристика гребёнки это затухающая череда импульсов
      -- через период задержки.
      testCase "даёт череду отражений" $ do
        let d = 100
            xs = render defaultEnv (comb (fromIntegral d / rate) 0.5 (impulse 1000))
            taps = [xs U.! (d * k) | k <- [0 .. 5]]
        assertBool (show taps) $
          and (zipWith (\a b -> abs (a - b) < 1e-12) taps [1, 0.5, 0.25, 0.125, 0.0625, 0.03125])
    , testCase "между отражениями тишина" $ do
        let xs = render defaultEnv (comb (100 / rate) 0.5 (impulse 1000))
        assertBool "не тишина" (all (\i -> abs (xs U.! i) < 1e-12) [1 .. 99])
    , -- Fx сохраняет длину, как и фильтры: хвост обрезается, а не звенит
      -- бесконечно.
      testCase "сохраняет длину" $ do
        U.length (render defaultEnv (comb 0.01 0.6 (impulse 500))) @?= 500
    , testCase "нулевой фидбэк не меняет сигнал" $ do
        render defaultEnv (comb 0.01 0 (impulse 500)) @?= render defaultEnv (impulse 500)
    , -- Фидбэк читается посэмплово: в момент первого отражения он нулевой,
      -- и отражение обязано пропасть.
      testCase "фидбэк читается посэмплово" $ do
        let fb = fromSamples (replicate 150 0 <> replicate 850 0.9)
            xs = render defaultEnv (comb (100 / rate) fb (impulse 1000))
            control = render defaultEnv (comb (100 / rate) 0.9 (impulse 1000))
        assertBool "эхо не задавлено" (abs (xs U.! 100) < 1e-12)
        assertBool "контроль" (abs (control U.! 100 - 0.9) < 1e-12)
    , testCase "не зависит от размера блока" $ do
        let big = render defaultEnv (comb 0.005 0.7 (impulse 3000))
            small = render defaultEnv {envBlock = 64} (comb 0.005 0.7 (impulse 3000))
        big @?= small
    ]

allpassTests :: TestTree
allpassTests =
  testGroup
    "allpass"
    [ -- Главное свойство: модуль отклика единица на всех частотах, меняется
      -- только фаза.
      testCase "модуль отклика единичный" $ do
        let n = 8192
            xs = render defaultEnv (allpass 0.002 0.7 (impulse n))
            mags = spectrum xs
            worst = maximum (map (\m -> abs (m - 1)) mags)
        assertBool (show worst) (worst < 1e-6)
    , testCase "фазу всё же меняет" $ do
        let xs = render defaultEnv (allpass 0.002 0.7 (impulse 1000))
        assertBool "сигнал не тронут" (abs (xs U.! 0 + 0.7) < 1e-12)
    , testCase "сохраняет длину" $ do
        U.length (render defaultEnv (allpass 0.002 0.7 (impulse 500))) @?= 500
    , testCase "не зависит от размера блока" $ do
        let big = render defaultEnv (allpass 0.002 0.7 (impulse 3000))
            small = render defaultEnv {envBlock = 64} (allpass 0.002 0.7 (impulse 3000))
        big @?= small
    ]
