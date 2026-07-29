-- | Нелинейности: шейперы, клиппер, редукция битов и частоты.
module NonlinSpec (tests) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Vector.Unboxed qualified as U
import Sound.Sig.Core
import Sound.Sig.Filter (highpass)
import Sound.Sig.Nonlin
import Sound.Sig.Osc (sine)
import Sound.Sig.Resample (oversample)
import Spectral (spectrum, strayDb)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Nonlin"
    [ shaperTests
    , cubicTests
    , clipTests
    , decimateTests
    , aliasTests
    ]

near :: String -> Double -> Double -> Double -> Assertion
near what tol expected got =
  assertBool (what <> ": ждали " <> show expected <> ", получили " <> show got) $
    abs (expected - got) <= tol

rate :: Double
rate = envRate defaultEnv

-- | Прогоняет значения через Fx.
through :: Fx -> [Double] -> [Double]
through fx xs = U.toList (render defaultEnv (fx (fromSamples xs)))

-- | Отклик на одиночное значение.
one :: Fx -> Double -> Double
one fx x = U.head (render defaultEnv (fx (fromSamples [x])))

-- Шейпер -------------------------------------------------------------------

shaperTests :: TestTree
shaperTests =
  testGroup
    "shaper"
    [ -- Нормировка tanh(d*x)/tanh(d): полная шкала остаётся полной шкалой.
      testCase "единица остаётся единицей" $ do
        near "shaper 5 на 1" 1e-12 1 (one (shaper 5) 1)
        near "shaper 5 на -1" 1e-12 (-1) (one (shaper 5) (-1))
    , testCase "ноль остаётся нулём" $ do
        near "shaper 5 на 0" 1e-12 0 (one (shaper 5) 0)
    , testCase "нечётная функция" $ do
        near "симметрия" 1e-12 (one (shaper 3) 0.4) (negate (one (shaper 3) (-0.4)))
    , testCase "поджимает середину диапазона вверх" $ do
        let y = one (shaper 5) 0.5
        assertBool (show y) (y > 0.5 && y < 1)
    , -- Предел при drive к нулю это тождество, а не деление нуля на ноль.
      testCase "нулевой drive не даёт NaN" $ do
        let ys = through (shaper 0) [0.3, -0.7]
        assertBool (show ys) (not (any isNaN ys))
        near "тождество" 1e-6 0.3 (one (shaper 0) 0.3)
    , testCase "drive это сигнал" $
        case through (shaper (fromSamples [0, 5])) [0.5, 0.5] of
          [a, b] -> assertBool (show (a, b)) (b > a)
          ys -> assertFailure (show ys)
    , -- Нормировка симметрична по знаку drive, но guard на малый drive
      -- смотрит на модуль. Без модуля любой отрицательный drive тихо
      -- превратил бы шейпер в тождество.
      testCase "знак drive не важен" $ do
        near "минус пять" 1e-12 (one (shaper 5) 0.3) (one (shaper (-5)) 0.3)
    ]

-- Кубический шейпер ---------------------------------------------------------

cubicTests :: TestTree
cubicTests =
  testGroup
    "cubicnl"
    [ testCase "без смещения нечётный" $ do
        near "симметрия" 1e-12 (one (cubicnl 1 0) 0.5) (negate (one (cubicnl 1 0) (-0.5)))
    , -- Смещение вносит постоянную составляющую, и она обязана вычитаться:
      -- иначе DC поедет дальше по тракту.
      testCase "смещение не даёт постоянной составляющей" $ do
        near "ноль на входе" 1e-12 0 (one (cubicnl 0.9 0.15) 0)
    , -- На переменном входе асимметричная кривая сама рождает постоянную
      -- составляющую, и вычитание константы её не убирает: среднее выходит
      -- около -0.75*drive^2*bias*A^2. Это свойство шейпера, а не недосмотр,
      -- поэтому после него нужен блокиратор DC.
      testCase "на переменном входе остаётся смещение по формуле" $ do
        let d = 1
            b = 0.3
            amp = 0.5
            xs = render defaultEnv (takeSec 0.1 (sine 200 * constant amp))
            ys = render defaultEnv (cubicnl (constant d) (constant b) (fromSamples (U.toList xs)))
            mean = U.sum ys / fromIntegral (U.length ys)
            want = negate (0.75 * d * d * b * amp * amp)
        near "среднее" (abs want * 0.05) want mean
    , -- И блокиратор действительно убирает: срез 20 Гц против сигнала 200.
      testCase "блокиратор DC убирает смещение" $ do
        let xs = render defaultEnv (takeSec 0.5 (sine 200 * 0.5))
            ys = render defaultEnv (highpass 20 (cubicnl 1 0.3 (fromSamples (U.toList xs))))
            tailOf = U.drop 4800 ys
            mean = U.sum tailOf / fromIntegral (U.length tailOf)
        assertBool ("среднее " <> show mean) (abs mean < 1e-3)
    , testCase "смещение ломает симметрию" $ do
        let a = one (cubicnl 0.9 0.15) 0.5
            b = one (cubicnl 0.9 0.15) (-0.5)
        assertBool (show (a, b)) (abs (a + b) > 1e-3)
    , testCase "за единицей выходит на полку" $ do
        let ys = through (cubicnl 1 0) [1, 2, 5]
        assertBool (show ys) (all (\v -> abs (v - 1) < 1e-12) ys)
    , -- Вычитание постоянной составляющей сдвигает диапазон: по одной полке
      -- выход уходит за единицу ровно на неё, по другой не доходит.
      testCase "выходит за шкалу не больше чем на смещение" $ do
        let b = 0.3
            dc = 1.5 * (b - b * b * b / 3)
            ys = through (cubicnl 2 (constant b)) [-1, -0.5, 0, 0.5, 1]
        assertBool (show ys) (all (\v -> abs v <= 1 + dc + 1e-12) ys)
        assertBool (show ys) (minimum ys < -1)
    ]

-- Клиппер -------------------------------------------------------------------

clipTests :: TestTree
clipTests =
  testGroup
    "clip"
    [ testCase "режет по порогу" $ do
        through (clip 0.5) [0, 0.2, 0.5, 0.9, -0.9] @?= [0, 0.2, 0.5, 0.5, -0.5]
    , testCase "внутри порога не трогает" $ do
        through (clip 1) [0.3, -0.7] @?= [0.3, -0.7]
    ]

-- Редукция ------------------------------------------------------------------

decimateTests :: TestTree
decimateTests =
  testGroup
    "decimate"
    [ -- Три бита это восемь уровней, шаг 1/4 от полной шкалы.
      testCase "квантует по битам" $ do
        through (decimate 3 1) [0, 0.3, 0.6, -0.3] @?= [0, 0.25, 0.5, -0.25]
    , testCase "16 бит почти прозрачны" $ do
        let ys = through (decimate 16 1) [0.3, -0.7]
        assertBool (show ys) (and (zipWith (\a b -> abs (a - b) < 1e-4) [0.3, -0.7] ys))
    , testCase "держит сэмпл заданное число раз" $ do
        through (decimate 16 3) [1, 2, 3, 4, 5, 6, 7] @?= [1, 1, 1, 4, 4, 4, 7]
    , testCase "удержание не зависит от размера блока" $ do
        let xs = [0 .. 99]
            big = through (decimate 16 7) xs
            small = U.toList (render defaultEnv {envBlock = 3} (decimate 16 7 (fromSamples xs)))
        big @?= small
    , testCase "неположительные параметры это ошибка" $ do
        errorsOn (decimate 0 1) >>= assertBool "биты"
        errorsOn (decimate 8 0) >>= assertBool "удержание"
    ]

-- | Падает ли Fx на простом входе.
errorsOn :: Fx -> IO Bool
errorsOn fx = do
  r <- try (evaluate (length (through fx [0.5, 0.5])))
  pure $ case r :: Either ErrorCall Int of
    Left _ -> True
    Right _ -> False

-- Алиасинг ------------------------------------------------------------------

-- | 3137 Гц вместо круглых 3000: у частот, делящих 48000 нацело, продукты
-- алиасинга падают ровно в бины гармоник и проверка их не видит.
probeHz :: Int
probeHz = 3137

-- | Худший не-гармонический бин относительно фундаментальной, дБ. Окно
-- ровно в секунду даёт бин в 1 Гц, поэтому все гармоники попадают в бины
-- точно и утечки нет. Начало отрезаем: там разгоняются фильтры oversample.
aliasOf :: Fx -> Double
aliasOf fx = strayDb probeHz (spectrum (U.slice 2000 (round rate) xs))
  where
    xs = render defaultEnv (takeSec 1.1 (fx (sine (constant (fromIntegral probeHz)))))

aliasTests :: TestTree
aliasTests =
  testGroup
    "алиасинг шейпера"
    [ -- Критерий M5: с оверсэмплингом продукты ниже -90 dBFS.
      testCase "oversample 8 (shaper 5) чист" $ do
        let db = aliasOf (oversample 8 (shaper 5))
        assertBool (show db <> " dB") (db < -90)
    , -- И контроль, что комбинатор реально работает: без него заметно хуже.
      testCase "без оверсэмплинга заметно грязнее" $ do
        let db = aliasOf (shaper 5)
        assertBool (show db <> " dB") (db > -60)
    ]
