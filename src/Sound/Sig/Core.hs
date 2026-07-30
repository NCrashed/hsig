-- share кэширует блоки через unsafePerformIO, поэтому GHC нельзя разрешать
-- вытаскивать эти выражения из-под лямбд и склеивать одинаковые.
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}

-- | Ядро: окружение рендера и сигнал как ленивый поток блоков.
--
-- Семантика длин разная у операций: (+) и (-) дополняют короткий сигнал
-- нулями, (*) и (/) обрезают до короткого, литералы бесконечны. Благодаря
-- этому огибающая задаёт длительность ноты, а микс не теряет хвосты.
module Sound.Sig.Core
  ( -- * Окружение
    Env (..)
  , defaultEnv

    -- * Сигнал
  , Sig (..)
  , Fx
  , Chunks

    -- * Построение
  , constant
  , fromSamples
  , blockOf

    -- * Выгрузка
  , samples
  , render

    -- * Комбинаторы
  , PadMode (..)
  , zipChunks
  , mapSig
  , takeSec
  , padSec
  , rechunk
  , share
  ) where

import Data.IORef (atomicModifyIORef', newIORef)
import Data.Vector.Unboxed qualified as U
import System.IO.Unsafe (unsafePerformIO)

-- | Параметры рендера. Sig это функция от Env, потому что oversample меняет
-- частоту дискретизации для поддерева графа.
data Env = Env
  { envRate :: !Double
  , envBlock :: !Int
  -- ^ размер блока в сэмплах, не меньше 1
  , envSeed :: !Int
  -- ^ база детерминированного шума и дизера
  , envMaxSec :: !Double
  -- ^ потолок длины стема и мастера в секундах: страховка от забытого окна
  -- (разд. 8). В ключ кэша не входит, звука не меняет. Короткому треку
  -- имеет смысл занизить: забытое окно тогда падает за секунду, а не через
  -- десять минут честного счёта.
  }
  deriving (Eq, Ord, Show)

defaultEnv :: Env
defaultEnv = Env {envRate = 48000, envBlock = 4096, envSeed = 0, envMaxSec = 600}

-- | Инвариант: все блоки кроме последнего длиной envBlock, последний короче
-- или равен, пустых блоков нет. Список может быть бесконечным.
type Chunks = [U.Vector Double]

newtype Sig = Sig {runSig :: Env -> Chunks}

type Fx = Sig -> Sig

-- Построение и выгрузка -------------------------------------------------

-- | Размер блока с проверкой: при envBlock меньше 1 нарезка ушла бы в
-- бесконечный поток пустых блоков, то есть в тихое зависание.
blockOf :: Env -> Int
blockOf env
  | b < 1 = error ("hsig: envBlock должен быть не меньше 1, задано " <> show b)
  | otherwise = b
  where
    b = envBlock env

-- | Бесконечный постоянный сигнал.
constant :: Double -> Sig
constant x = Sig $ \env -> repeat (U.replicate (blockOf env) x)

-- | Сигнал из готовых сэмплов, длиной ровно со список.
fromSamples :: [Double] -> Sig
fromSamples xs0 = Sig $ \env -> go (blockOf env) xs0
  where
    go _ [] = []
    go n xs = let (h, t) = splitAt n xs in U.fromListN n h : go n t

-- | Разворачивает сигнал в поток сэмплов. Для бесконечного сигнала брать
-- префикс.
samples :: Env -> Sig -> [Double]
samples env sig = concatMap U.toList (runSig sig env)

-- | Собирает конечный сигнал в один вектор.
render :: Env -> Sig -> U.Vector Double
render env sig = U.concat (runSig sig env)

-- Комбинаторы -----------------------------------------------------------

-- | Что делать, когда один из сигналов кончился раньше.
data PadMode
  = -- | добить нулями до длины длинного
    PadZero
  | -- | обрезать до длины короткого
    Truncate
  deriving (Eq, Show)

-- | Поэлементная бинарная операция. Границы блоков у аргументов совпадают,
-- пока один из них не кончится, поэтому результат перебивается заново.
zipChunks :: PadMode -> (Double -> Double -> Double) -> Sig -> Sig -> Sig
zipChunks mode f (Sig ga) (Sig gb) = Sig $ \env ->
  rechunk (blockOf env) (go (ga env) (gb env))
  where
    tail' g = case mode of
      Truncate -> []
      PadZero -> g
    go [] [] = []
    go as [] = tail' (map (U.map (`f` 0)) as)
    go [] bs = tail' (map (U.map (f 0)) bs)
    go (a : as) (b : bs)
      | la == lb = U.zipWith f a b : go as bs
      | la < lb = U.zipWith f a (U.take la b) : go as (U.drop la b : bs)
      | otherwise = U.zipWith f (U.take lb a) b : go (U.drop lb a : as) bs
      where
        la = U.length a
        lb = U.length b

-- | Поэлементная унарная операция.
mapSig :: (Double -> Double) -> Fx
mapSig f (Sig g) = Sig $ \env -> map (U.map f) (g env)

-- | Перебивает поток на блоки по n сэмплов, восстанавливая инвариант.
rechunk :: Int -> Chunks -> Chunks
rechunk n
  | n < 1 = error ("hsig: размер блока должен быть не меньше 1, задано " <> show n)
  | otherwise = go U.empty
  where
    go buf cs
      | U.length buf >= n = U.take n buf : go (U.drop n buf) cs
      | otherwise = case cs of
          [] -> [buf | not (U.null buf)]
          c : rest
            -- Ходовой случай: блоки уже выровнены, копировать нечего.
            | U.null buf && U.length c == n -> c : go buf rest
            | otherwise -> go (buf U.++ c) rest

-- | Обрезает сигнал до длительности в секундах.
takeSec :: Double -> Fx
takeSec t (Sig g) = Sig $ \env -> go (max 0 (round (t * envRate env))) (g env)
  where
    go n cs
      | n <= 0 = []
      | otherwise = case cs of
          [] -> []
          c : rest
            | U.length c >= n -> [U.take n c]
            | otherwise -> c : go (n - U.length c) rest

-- | Добивает сигнал нулями до длительности в секундах. Длинный не режет.
padSec :: Double -> Fx
padSec t (Sig g) = Sig $ \env ->
  let block = blockOf env
      zeros n
        | n <= 0 = []
        | otherwise = U.replicate (min block n) 0 : zeros (n - block)
      go n cs
        | n <= 0 = cs
        | otherwise = case cs of
            [] -> zeros n
            c : rest -> c : go (n - U.length c) rest
   in rechunk block (go (round (t * envRate env)) (g env))

-- | Мемоизирует блоки узла по Env: без этого @x + x@ считает @x@ дважды, а
-- на глубоком графе это экспоненциальный взрыв. Любой Sig, использованный в
-- определении больше одного раза, оборачивать в share.
--
-- Цена: кэш держит голову потока, поэтому пройденные блоки живут, пока жив
-- сам узел, то есть память порядка длины сигнала. Для инструмента это длина
-- ноты, а стемы по разд. 8 идут на диск, поэтому в рамках дизайна это
-- приемлемо; оборачивать в share сигнал длиной в трек не надо.
--
-- Исключение - узел, питающий несколько стемов (бочка, от которой качаются
-- остальные): там 12 МБ на 32 секундах дешевле, чем пять её пересчётов.
share :: Sig -> Sig
share (Sig g) = unsafePerformIO $ do
  cache <- newIORef []
  pure $ Sig $ \env -> unsafePerformIO $ atomicModifyIORef' cache $ \entries ->
    case lookup env entries of
      Just cs -> (entries, cs)
      -- Разные Env приходят только из вложенных oversample, поэтому пары
      -- записей хватает; ограничение держит память под контролем.
      Nothing -> let cs = g env in (take 4 ((env, cs) : entries), cs)
{-# NOINLINE share #-}

-- Числовые инстансы -----------------------------------------------------

instance Num Sig where
  (+) = zipChunks PadZero (+)
  (-) = zipChunks PadZero (-)
  (*) = zipChunks Truncate (*)
  negate = mapSig negate
  abs = mapSig abs
  signum = mapSig signum
  fromInteger = constant . fromInteger

instance Fractional Sig where
  (/) = zipChunks Truncate (/)
  recip = mapSig recip
  fromRational = constant . fromRational

instance Floating Sig where
  pi = constant pi
  exp = mapSig exp
  log = mapSig log
  sqrt = mapSig sqrt
  sin = mapSig sin
  cos = mapSig cos
  tan = mapSig tan
  asin = mapSig asin
  acos = mapSig acos
  atan = mapSig atan
  sinh = mapSig sinh
  cosh = mapSig cosh
  tanh = mapSig tanh
  asinh = mapSig asinh
  acosh = mapSig acosh
  atanh = mapSig atanh
  (**) = zipChunks Truncate (**)
  logBase = zipChunks Truncate logBase
