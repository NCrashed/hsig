-- | Ядро: блочный инвариант, семантика длин, share.
module CoreSpec (tests) where

import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad (void)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (unsnoc)
import Data.Vector.Unboxed qualified as U
import Sound.Sig.Core
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Core"
    [ blockInvariant
    , lengths
    , arithmetic
    , numeric
    , slicing
    , edges
    , laziness
    , sharing
    ]

-- Генераторы -----------------------------------------------------------

-- | Сэмплы вида k/128 точно представимы в Double, поэтому ассоциативность
-- сложения проверяется без допуска.
newtype Sample = Sample {unSample :: Double}
  deriving (Eq, Show)

instance Arbitrary Sample where
  arbitrary = Sample . (/ 128) . fromIntegral <$> chooseInt (-128, 127)

-- | Мелкий размер блока гоняет код по границам чанков.
newtype Block = Block Int
  deriving (Eq, Show)

instance Arbitrary Block where
  arbitrary = Block <$> chooseInt (1, 8)

envOf :: Block -> Env
envOf (Block b) = defaultEnv {envBlock = b}

sigOf :: [Sample] -> Sig
sigOf = fromSamples . map unSample

-- Блочный инвариант ----------------------------------------------------

-- | Все блоки кроме последнего ровно envBlock, последний короче или равен,
-- пустых блоков нет.
holds :: Env -> Chunks -> Bool
holds env cs = case unsnoc cs of
  Nothing -> True
  Just (front, back) ->
    all ((== envBlock env) . U.length) front
      && U.length back > 0
      && U.length back <= envBlock env

blockInvariant :: TestTree
blockInvariant =
  testGroup
    "блочный инвариант"
    [ testProperty "fromSamples" $ \b xs ->
        let env = envOf b in holds env (runSig (sigOf xs) env)
    , testProperty "сложение" $ \b xs ys ->
        let env = envOf b in holds env (runSig (sigOf xs + sigOf ys) env)
    , testProperty "вычитание" $ \b xs ys ->
        let env = envOf b in holds env (runSig (sigOf xs - sigOf ys) env)
    , testProperty "умножение" $ \b xs ys ->
        let env = envOf b in holds env (runSig (sigOf xs * sigOf ys) env)
    , testProperty "деление" $ \b xs ys ->
        let env = envOf b in holds env (runSig (sigOf xs / sigOf ys) env)
    , testProperty "mapSig" $ \b xs ->
        let env = envOf b in holds env (runSig (mapSig negate (sigOf xs)) env)
    , testProperty "share" $ \b xs ->
        let env = envOf b in holds env (runSig (share (sigOf xs)) env)
    , testProperty "takeSec" $ \b xs (Positive t) ->
        let env = envOf b in holds env (runSig (takeSec t (sigOf xs)) env)
    , testProperty "padSec" $ \b xs (Positive t) ->
        let env = envOf b in holds env (runSig (padSec t (sigOf xs)) env)
    , testProperty "rechunk" $ \b xs ->
        let env = envOf b
            ragged = map (U.fromList . map unSample) xs
         in holds env (rechunk (envBlock env) ragged)
    ]

-- Длины ----------------------------------------------------------------

lengths :: TestTree
lengths =
  testGroup
    "длины"
    [ testProperty "fromSamples сохраняет сэмплы" $ \b xs ->
        samples (envOf b) (sigOf xs) === map unSample xs
    , testProperty "сложение дополняет до максимума" $ \b xs ys ->
        length (samples (envOf b) (sigOf xs + sigOf ys))
          === max (length xs) (length ys)
    , testProperty "умножение обрезает до минимума" $ \b xs ys ->
        length (samples (envOf b) (sigOf xs * sigOf ys))
          === min (length xs) (length ys)
    ]

-- Арифметика -----------------------------------------------------------

zipPad :: (Double -> Double -> Double) -> [Double] -> [Double] -> [Double]
zipPad f (x : xs) (y : ys) = f x y : zipPad f xs ys
zipPad f xs [] = map (`f` 0) xs
zipPad f [] ys = map (f 0) ys

arithmetic :: TestTree
arithmetic =
  testGroup
    "арифметика"
    [ testProperty "сложение поэлементно с добивкой нулями" $ \b xs ys ->
        samples (envOf b) (sigOf xs + sigOf ys)
          === zipPad (+) (map unSample xs) (map unSample ys)
    , -- Вычитание ловит добивку не с той стороны: f 0 y, а не f y 0.
      testProperty "вычитание поэлементно с добивкой нулями" $ \b xs ys ->
        samples (envOf b) (sigOf xs - sigOf ys)
          === zipPad (-) (map unSample xs) (map unSample ys)
    , testProperty "умножение поэлементно на общем префиксе" $ \b xs ys ->
        samples (envOf b) (sigOf xs * sigOf ys)
          === zipWith (*) (map unSample xs) (map unSample ys)
    , testProperty "сложение коммутативно" $ \b xs ys ->
        samples (envOf b) (sigOf xs + sigOf ys)
          === samples (envOf b) (sigOf ys + sigOf xs)
    , testProperty "сложение ассоциативно" $ \b xs ys zs ->
        samples (envOf b) ((sigOf xs + sigOf ys) + sigOf zs)
          === samples (envOf b) (sigOf xs + (sigOf ys + sigOf zs))
    , testProperty "x * 1 == x" $ \b xs ->
        samples (envOf b) (sigOf xs * 1) === map unSample xs
    , -- Расхождение с DESIGN.md, разд. 12: 0 это бесконечная константа
      -- (разд. 3), поэтому x + 0 бесконечен, а не равен x по длине.
      -- Совпадение проверяем на префиксе длиной x.
      testProperty "x + 0 равен x на своём префиксе" $ \b xs ->
        take (length xs) (samples (envOf b) (sigOf xs + 0)) === map unSample xs
    , testCase "хвост x + 0 это нули" $ do
        let env = defaultEnv {envBlock = 4}
            got = samples env (takeSec (10 / envRate env) (fromSamples [1, 2, 3] + 0))
        got @?= [1, 2, 3, 0, 0, 0, 0, 0, 0, 0]
    , testProperty "функции применяются поэлементно" $ \b xs ->
        samples (envOf b) (mapSig negate (sigOf xs)) === map (negate . unSample) xs
    ]

-- Fractional и Floating -------------------------------------------------

-- | Делители без нулей: сравнение с эталоном должно быть точным, а NaN
-- не равен сам себе.
nonZero :: [Sample] -> [Double]
nonZero = map (\(Sample v) -> if v == 0 then 1 else v)

numeric :: TestTree
numeric =
  testGroup
    "Fractional и Floating"
    [ testProperty "деление поэлементно на общем префиксе" $ \b xs ys ->
        samples (envOf b) (sigOf xs / fromSamples (nonZero ys))
          === zipWith (/) (map unSample xs) (nonZero ys)
    , testProperty "деление обрезает до минимума" $ \b xs ys ->
        length (samples (envOf b) (sigOf xs / fromSamples (nonZero ys)))
          === min (length xs) (length ys)
    , testProperty "recip поэлементно" $ \b xs ->
        samples (envOf b) (recip (fromSamples (nonZero xs))) === map recip (nonZero xs)
    , testCase "дробный литерал бесконечен" $ do
        take 5 (samples defaultEnv 0.25) @?= replicate 5 0.25
    , -- sine идёт через Prelude.sin внутри mapSig, поэтому метод Floating
      -- для Sig иначе не выполнялся бы ни разу.
      testProperty "sin через инстанс Floating поэлементно" $ \b xs ->
        samples (envOf b) (sin (sigOf xs)) === map (sin . unSample) xs
    , testProperty "sqrt поэлементно" $ \b xs ->
        samples (envOf b) (sqrt (mapSig abs (sigOf xs)))
          === map (sqrt . abs . unSample) xs
    , testCase "pi бесконечная константа" $ do
        take 3 (samples defaultEnv pi) @?= replicate 3 (pi :: Double)
    , testProperty "logBase не путает основание и аргумент" $ \b xs ->
        let vals = map (\(Sample v) -> abs v + 2) xs
         in samples (envOf b) (logBase 2 (fromSamples vals))
              === map (logBase 2) vals
    , testProperty "возведение в степень обрезает до минимума" $ \b xs ys ->
        length (samples (envOf b) (mapSig abs (sigOf xs) ** sigOf ys))
          === min (length xs) (length ys)
    ]

-- Границы --------------------------------------------------------------

edges :: TestTree
edges =
  testGroup
    "границы"
    [ testCase "пустой сигнал не даёт блоков" $ do
        runSig (fromSamples []) defaultEnv @?= []
    , testCase "пустой слева не съедает сложение" $ do
        samples defaultEnv (fromSamples [] + fromSamples [1, 2]) @?= [1, 2]
    , testCase "пустой справа меняет знак у вычитания" $ do
        samples defaultEnv (fromSamples [] - fromSamples [1, 2]) @?= [-1, -2]
    , testCase "умножение на пустой даёт пустой" $ do
        samples defaultEnv (fromSamples [] * fromSamples [1, 2]) @?= []
    , testCase "takeSec нулевой длительности" $ do
        samples defaultEnv (takeSec 0 (constant 1)) @?= []
    , testCase "блок в один сэмпл" $ do
        runSig (fromSamples [1, 2]) defaultEnv {envBlock = 1}
          @?= [U.singleton 1, U.singleton 2]
    , -- Нулевой блок нарезал бы бесконечный поток пустых блоков, то есть
      -- подвесил бы рендер без единого сообщения.
      testCase "envBlock меньше 1 это ошибка, а не зависание" $ do
        r <-
          try (evaluate (length (samples defaultEnv {envBlock = 0} (fromSamples [1, 2]))))
            :: IO (Either ErrorCall Int)
        case r of
          Left _ -> pure ()
          Right n -> assertFailure ("ожидали ошибку, получили " <> show n)
    ]

-- Ленивость -------------------------------------------------------------

-- | Поток, у которого форсирование хвоста падает: ловит потерю ленивости
-- быстрым fail вместо зависания.
poison :: Sig
poison = Sig $ \env -> U.replicate (envBlock env) 1 : error "хвост потока форсирован"

lazyEnv :: Env
lazyEnv = defaultEnv {envBlock = 4}

laziness :: TestTree
laziness =
  testGroup
    "ленивость"
    [ testCase "сложение не заглядывает за первый блок" $ do
        take 4 (samples lazyEnv (poison + poison)) @?= [2, 2, 2, 2]
    , testCase "умножение не заглядывает за первый блок" $ do
        take 4 (samples lazyEnv (poison * poison)) @?= [1, 1, 1, 1]
    , testCase "mapSig не заглядывает за первый блок" $ do
        take 4 (samples lazyEnv (mapSig negate poison)) @?= [-1, -1, -1, -1]
    , testCase "takeSec не заглядывает за нужный блок" $ do
        samples lazyEnv (takeSec (4 / envRate lazyEnv) poison) @?= [1, 1, 1, 1]
    , testCase "бесконечные сигналы складываются" $ do
        take 3 (samples defaultEnv (constant 1 + constant 2)) @?= [3, 3, 3]
    ]

-- Резка ----------------------------------------------------------------

slicing :: TestTree
slicing =
  testGroup
    "резка"
    [ testCase "takeSec отмеряет секунды по частоте" $ do
        let env = defaultEnv
        length (samples env (takeSec 2 (constant 1))) @?= 96000
    , testCase "takeSec короче сигнала не удлиняет его" $ do
        let env = defaultEnv {envBlock = 4}
        samples env (takeSec 1 (fromSamples [1, 2, 3])) @?= [1, 2, 3]
    , testCase "padSec добивает нулями" $ do
        let env = defaultEnv {envBlock = 4, envRate = 10}
        samples env (padSec 1 (fromSamples [1, 2, 3])) @?= [1, 2, 3, 0, 0, 0, 0, 0, 0, 0]
    , testCase "padSec не режет длинный сигнал" $ do
        let env = defaultEnv {envBlock = 4, envRate = 10}
        samples env (padSec 0.2 (fromSamples [1, 2, 3])) @?= [1, 2, 3]
    , testCase "render собирает сигнал в вектор" $ do
        render defaultEnv (takeSec 0.001 (constant 0.5))
          @?= U.replicate 48 0.5
    ]

-- share ----------------------------------------------------------------

-- | Считает, сколько раз с узла запрашивали блоки.
counting :: IORef Int -> Sig
counting ref = Sig $ \env -> unsafePerformIO $ do
  modifyIORef' ref (+ 1)
  pure (runSig (constant 1) env)

force :: Sig -> IO ()
force sig = void (evaluate (sum (take 8 (samples defaultEnv sig))))

sharing :: TestTree
sharing =
  testGroup
    "share"
    [ testCase "без share узел считается дважды" $ do
        ref <- newIORef 0
        let s = counting ref
        force (s + s)
        readIORef ref >>= (@?= 2)
    , testCase "с share узел считается один раз" $ do
        ref <- newIORef 0
        let s = share (counting ref)
        force (s + s)
        readIORef ref >>= (@?= 1)
    , testCase "кэш ключуется по Env" $ do
        let s = share (takeSec 1 (constant 1))
        length (samples defaultEnv s) @?= 48000
        length (samples defaultEnv {envRate = 44100} s) @?= 44100
    ]
