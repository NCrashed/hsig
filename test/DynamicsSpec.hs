-- | Динамика: компрессор и сайдчейн.
module DynamicsSpec (tests) where

import Data.Vector.Unboxed qualified as U
import Sound.Sig.Core
import Sound.Sig.Dynamics
import Sound.Sig.Osc (sine)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Dynamics"
    [ compressTests
    , gainTests
    , sidechainTests
    ]

rate :: Double
rate = envRate defaultEnv

-- | Установившееся значение: смотрим ближе к концу, когда следящая
-- огибающая уже догнала вход.
settled :: U.Vector Double -> Double
settled xs = U.sum tailPart / fromIntegral (U.length tailPart)
  where
    tailPart = U.drop (U.length xs - 1000) xs

compressTests :: TestTree
compressTests =
  testGroup
    "compress"
    [ -- Порог 0.2, отношение 4, вход 1: на выходе 0.2 + 0.8/4 = 0.4.
      testCase "сжимает по отношению" $ do
        let xs = render defaultEnv (compress 0.2 4 0.001 0.05 (takeSec 0.5 (constant 1)))
        assertBool (show (settled xs)) (abs (settled xs - 0.4) < 1e-3)
    , testCase "другое отношение даёт другой уровень" $ do
        let xs = render defaultEnv (compress 0.2 2 0.001 0.05 (takeSec 0.5 (constant 1)))
        assertBool (show (settled xs)) (abs (settled xs - 0.6) < 1e-3)
    , testCase "ниже порога не трогает" $ do
        let xs = render defaultEnv (compress 0.5 4 0.001 0.05 (takeSec 0.2 (constant 0.3)))
        assertBool (show (settled xs)) (abs (settled xs - 0.3) < 1e-9)
    , testCase "отношение 1 это тождество" $ do
        let src = takeSec 0.1 (sine 440)
            xs = render defaultEnv (compress 0.1 1 0.001 0.05 src)
            want = render defaultEnv src
        assertBool "разошлось" (U.maximum (U.map abs (U.zipWith (-) xs want)) < 1e-12)
    , -- Атака за постоянную времени проходит примерно 63 процента пути.
      testCase "атака идёт по постоянной времени" $ do
        let att = 0.01
            xs = render defaultEnv (compress 0.2 4 att 0.05 (takeSec 0.2 (constant 1)))
            atT = xs U.! round (att * rate)
        assertBool (show atT) (atT > 0.4 && atT < 0.75)
    , testCase "быстрая атака сжимает раньше медленной" $ do
        let quick = render defaultEnv (compress 0.2 4 0.001 0.05 (takeSec 0.2 (constant 1)))
            slow = render defaultEnv (compress 0.2 4 0.05 0.05 (takeSec 0.2 (constant 1)))
            at = round (0.005 * rate)
        assertBool "не быстрее" (quick U.! at < slow U.! at)
    , testCase "не зависит от размера блока" $ do
        let src = takeSec 0.2 (sine 220)
            big = render defaultEnv (compress 0.2 4 0.005 0.05 src)
            small = render defaultEnv {envBlock = 61} (compress 0.2 4 0.005 0.05 src)
        assertBool "разошлось" (U.maximum (U.map abs (U.zipWith (-) big small)) < 1e-15)
    , -- Вырожденные параметры зажимаются, а не дают NaN. В знаменателе
      -- усиления стоят отношение и сама огибающая.
      testCase "вырожденные параметры не дают NaN" $ do
        let run t r a rl = render defaultEnv (compress t r a rl (takeSec 0.05 (constant 1)))
            finite = U.all (\v -> not (isNaN v) && not (isInfinite v))
        assertBool "нулевой порог" (finite (run 0 4 0.005 0.05))
        assertBool "отрицательный порог" (finite (run (-1) 4 0.005 0.05))
        assertBool "нулевое отношение" (finite (run 0.2 0 0.005 0.05))
        assertBool "нулевые времена" (finite (run 0.2 4 0 0))
        assertBool "тишина на входе" (finite (render defaultEnv (compress 0.2 4 0.005 0.05 (takeSec 0.05 0))))
    , -- Тонкий случай: при неположительном пороге даже тишина считается
      -- превышением, и при отношении 1 числитель усиления тоже обращается в
      -- ноль. Без зажима порога это ровно 0/0.
      testCase "отрицательный порог на тишине не даёт NaN" $ do
        let xs = render defaultEnv (compress (-1) 1 0.005 0.05 (takeSec 0.05 0))
        assertBool "NaN" (U.all (not . isNaN) xs)
    , -- Отношение меньше единицы это расширение, а не ошибка.
      testCase "отношение меньше единицы расширяет" $ do
        let xs = render defaultEnv (compress 0.2 0.5 0.001 0.05 (takeSec 0.5 (constant 0.5)))
        assertBool (show (settled xs)) (settled xs > 0.5)
    ]

gainTests :: TestTree
gainTests =
  testGroup
    "compressGain"
    [ -- Определяющее соотношение: компрессор это вход, умноженный на свой же
      -- коэффициент. Побитово, иначе стерео-вариант звучал бы иначе моно.
      testCase "compress это вход на свой коэффициент" $ do
        let src = takeSec 0.3 (sine 220 * 0.8)
            xs = render defaultEnv (compress 0.2 4 0.005 0.05 src)
            ys = render defaultEnv (src * compressGain 0.2 4 0.005 0.05 src)
        xs @?= ys
    , testCase "ниже порога коэффициент единичный" $ do
        let xs = render defaultEnv (compressGain 0.5 4 0.001 0.05 (takeSec 0.2 (constant 0.3)))
        assertBool (show (U.maximum xs)) (U.all (== 1) xs)
    , -- Порог 0.2, отношение 4, вход 1: коэффициент выходит на 0.4.
      testCase "коэффициент идёт по отношению" $ do
        let xs = render defaultEnv (compressGain 0.2 4 0.001 0.05 (takeSec 0.5 (constant 1)))
        assertBool (show (settled xs)) (abs (settled xs - 0.4) < 1e-3)
    , -- Ради чего это и заведено. Каналы намеренно не кратны друг другу:
      -- у кратных отношение сохранил бы любой общий коэффициент, и тест
      -- проходил бы на сломанном compressGain.
      testCase "связанный коэффициент сохраняет баланс каналов" $ do
        let l = takeSec 0.4 (sine 220 * 0.9)
            r = takeSec 0.4 (sine 330 * 0.25)
            g = compressGain 0.2 4 0.005 0.05 (l + r)
            ratio xs ys = settled (U.map abs xs) / settled (U.map abs ys)
            out = render defaultEnv
            want = ratio (out l) (out r)
            linked = ratio (out (l * g)) (out (r * g))
            apart =
              ratio
                (out (compress 0.2 4 0.005 0.05 l))
                (out (compress 0.2 4 0.005 0.05 r))
        assertBool (show (want, linked)) (abs (linked / want - 1) < 0.02)
        -- Раздельный давит громкий канал сильнее, и баланс уезжает.
        assertBool (show (want, apart)) (apart / want < 0.8)
    , -- Длина у коэффициента своя, от управляющего сигнала: короткий
      -- управляющий обрежет вход, а не пропустит хвост.
      testCase "длина коэффициента это длина управляющего" $ do
        let g = compressGain 0.2 4 0.005 0.05 (takeSec 0.1 (constant 1))
        U.length (render defaultEnv g) @?= round (0.1 * rate)
        U.length (render defaultEnv (takeSec 0.3 (sine 220) * g)) @?= round (0.1 * rate)
    ]

sidechainTests :: TestTree
sidechainTests =
  testGroup
    "sidechain"
    [ -- Управляющий сигнал молчит: вход не трогаем.
      testCase "тишина в управлении не давит" $ do
        let xs = render defaultEnv (sidechain (constant 0) 0.7 (takeSec 0.2 (constant 1)))
        assertBool (show (settled xs)) (abs (settled xs - 1) < 1e-6)
    , -- Глубина 0.7 при полном управляющем сигнале давит до 0.3.
      testCase "полный управляющий сигнал давит на глубину" $ do
        let xs = render defaultEnv (sidechain (constant 1) 0.7 (takeSec 0.5 (constant 1)))
        assertBool (show (settled xs)) (abs (settled xs - 0.3) < 1e-2)
    , -- Управляющий сигнал не обязан держаться в шкале: суммы и стеки легко
      -- вылезают за единицу. Без зажима множитель ушёл бы в минус, то есть
      -- в инверсию фазы с усилением вместо приглушения.
      testCase "громкий управляющий сигнал не инвертирует" $ do
        let xs = render defaultEnv (sidechain (constant 3) 0.9 (takeSec 0.3 (constant 1)))
        assertBool (show (settled xs)) (settled xs > 0)
        assertBool (show (settled xs)) (abs (settled xs - 0.1) < 1e-2)
    , testCase "нулевая глубина ничего не меняет" $ do
        let src = takeSec 0.1 (sine 440)
            xs = render defaultEnv (sidechain (constant 1) 0 src)
            want = render defaultEnv src
        assertBool "разошлось" (U.maximum (U.map abs (U.zipWith (-) xs want)) < 1e-12)
    , -- Удар в управлении даёт провал, после которого уровень возвращается.
      testCase "удар даёт провал и возврат" $ do
        let hit = fromSamples (replicate 480 1 <> replicate 23520 0)
            xs = render defaultEnv (sidechain hit 0.8 (takeSec 0.5 (constant 1)))
            dip = xs U.! 400
            back = xs U.! 20000
        assertBool ("провал " <> show dip) (dip < 0.4)
        assertBool ("возврат " <> show back) (back > 0.95)
    , testCase "длина по входу" $ do
        U.length (render defaultEnv (sidechain (constant 1) 0.5 (takeSec 0.1 (constant 1))))
          @?= round (0.1 * rate)
    , testCase "не зависит от размера блока" $ do
        let src = takeSec 0.2 (sine 220)
            ctrl = takeSec 0.2 (sine 5)
            big = render defaultEnv (sidechain ctrl 0.7 src)
            small = render defaultEnv {envBlock = 61} (sidechain ctrl 0.7 src)
        assertBool "разошлось" (U.maximum (U.map abs (U.zipWith (-) big small)) < 1e-15)
    ]
