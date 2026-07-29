-- | Внутренняя поблочная машинерия: выравнивание потоков и пробег по блоку
-- с состоянием. Наружу из библиотеки не выставляется.
module Sound.Sig.Block
  ( align2
  , align2Pad
  , align3
  , sweep
  , stateful1
  ) where

import Control.Monad.ST (runST)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Sound.Sig.Core (Chunks, Fx, Sig (..))

-- | Обработка с состоянием без управляющих сигналов. Длину сохраняет, а
-- значит и инвариант блоков.
stateful1 :: s -> (s -> Double -> (Double, s)) -> Fx
stateful1 s0 step (Sig g) = Sig $ \env -> go s0 (g env)
  where
    go _ [] = []
    go !s (c : cs) = out : go s' cs
      where
        (out, s') = sweep (\st i -> step st (U.unsafeIndex c i)) (U.length c) s

-- | Пробегает блок с состоянием. Рекурсия по сэмплам живёт здесь, наружу
-- торчит только Fx (разд. 2 дизайна).
sweep :: (s -> Int -> (Double, s)) -> Int -> s -> (U.Vector Double, s)
sweep f n s0 = runST $ do
  out <- UM.new n
  let loop !i !s
        | i >= n = pure s
        | otherwise = do
            let (y, s') = f s i
            UM.unsafeWrite out i y
            loop (i + 1) s'
  sEnd <- loop 0 s0
  v <- U.unsafeFreeze out
  pure (v, sEnd)

-- | Остаток блока после отрезанных n сэмплов.
keep :: Int -> U.Vector Double -> Chunks -> Chunks
keep n v vs
  | U.length v > n = U.drop n v : vs
  | otherwise = vs

-- | Выравнивает два потока по общим границам, обрезая по короткому.
align2 :: Chunks -> Chunks -> [(U.Vector Double, U.Vector Double)]
align2 (a : as) (b : bs) = (U.take n a, U.take n b) : align2 (keep n a as) (keep n b bs)
  where
    n = min (U.length a) (U.length b)
align2 _ _ = []

-- | То же, но короткий поток дополняется нулями.
--
-- Для управляющего сигнала обрезка по короткому верна (так же работает
-- (*)), а вот для двух равноправных каналов нет: там хвост длинного это
-- звук, и терять его нельзя. Семантика совпадает с (+), то есть PadZero.
align2Pad :: Chunks -> Chunks -> [(U.Vector Double, U.Vector Double)]
align2Pad (a : as) (b : bs) = (U.take n a, U.take n b) : align2Pad (keep n a as) (keep n b bs)
  where
    n = min (U.length a) (U.length b)
align2Pad as [] = [(a, zeros a) | a <- as]
align2Pad [] bs = [(zeros b, b) | b <- bs]

zeros :: U.Vector Double -> U.Vector Double
zeros v = U.replicate (U.length v) 0

-- | То же для трёх потоков.
align3 :: Chunks -> Chunks -> Chunks -> [(U.Vector Double, U.Vector Double, U.Vector Double)]
align3 (a : as) (b : bs) (c : cs) =
  (U.take n a, U.take n b, U.take n c) : align3 (keep n a as) (keep n b bs) (keep n c cs)
  where
    n = min (U.length a) (min (U.length b) (U.length c))
align3 _ _ _ = []
