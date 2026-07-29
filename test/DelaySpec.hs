-- | Задержки: линия, гребёнка, фазовый фильтр.
module DelaySpec (tests) where

import Data.Vector.Unboxed qualified as U
import Sound.Sig.Core
import Sound.Sig.Delay
import Spectral (spectrum)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Delay"
    [ delayTests
    , combTests
    , allpassTests
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
