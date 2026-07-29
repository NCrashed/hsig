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
