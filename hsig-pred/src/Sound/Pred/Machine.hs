-- | Явная эпсилон-машина и её инварианты.
--
-- Машина унифилярна по построению: следующее состояние определено текущим
-- состоянием и излучённым символом. Это не удобство представления, а само
-- определение причинного состояния - класса прошлых с одинаковым
-- распределением будущих.
--
-- Из унифилярности следует, что энтропийная скорость считается как среднее
-- энтропий выхода по стационарному весу, без предельного перехода по
-- длине блока.
module Sound.Pred.Machine
  ( Machine (..)
  , toPred
  , stationary
  , entropyRate
  , statComplexity
  ) where

import Sound.Pred.Dist
import Sound.Pred.Model (Pred, unfoldPred)

-- | Конечная унифилярная машина: старт, состояния, выход и переход.
data Machine s a = Machine
  { machineStart :: s
  , machineStates :: [s]
  , machineOut :: s -> Dist a
  , machineStep :: s -> a -> s
  }

-- | Забыть про состояния и смотреть только на поведение.
toPred :: Machine s a -> Pred a
toPred m = unfoldPred (machineOut m) (machineStep m) (machineStart m)

-- | Стационарное распределение по причинным состояниям.
--
-- Считается степенным методом по ленивой цепи (полшага на месте, полшага
-- вперёд). Демпфирование обязательно: без него машина с периодом два
-- колеблется вечно вместо сходимости.
stationary :: (Ord s) => Machine s a -> Dist s
stationary m = go (0 :: Int) (uniform (machineStates m))
  where
    go i d
      | i >= maxIter = d
      | totalVariation d d' < tol = d'
      | otherwise = go (i + 1) d'
      where
        d' = mix [(0.5, d), (0.5, push d)]
    push d =
      dist
        [ (machineStep m s x, ps * px)
        | (s, ps) <- distPairs d
        , (x, px) <- distPairs (machineOut m s)
        ]
    tol = 1e-15
    maxIter = 100000

-- | Энтропийная скорость @h_mu@ в битах на символ: неустранимая
-- непредсказуемость процесса.
entropyRate :: (Ord s) => Machine s a -> Double
entropyRate m = sum [p * entropy (machineOut m s) | (s, p) <- distPairs (stationary m)]

-- | Статистическая сложность @C_mu@ в битах: сколько памяти нужно, чтобы
-- предсказывать оптимально.
--
-- Это не «сколько состояний», а энтропия стационарного распределения по
-- ним: редко посещаемое состояние стоит дёшево.
statComplexity :: (Ord s) => Machine s a -> Double
statComplexity = entropy . stationary
