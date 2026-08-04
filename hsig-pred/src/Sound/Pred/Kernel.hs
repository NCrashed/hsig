-- | Крошечные порождающие ядра.
--
-- Каждое ядро это несколько правил, поведение бесконечно. Это и есть
-- рабочая гипотеза подпакета: интересное должно иметь маленькое ядро,
-- потому что пропускная способность канала до модели слушателя мала
-- (docs/PRED.md, разд. 7).
module Sound.Pred.Kernel
  ( -- * Подстановки
    Rules
  , substWord
  , substPred
  , thueMorse
  , periodDoubling
  , fibonacci
  , thueMorseWord
  , periodDoublingWord
  , fibonacciWord

    -- * Двухсостоятельные процессы
  , TwoState (..)
  , evenProcess
  , goldenMean
  ) where

import Data.Map.Strict qualified as M
import Sound.Pred.Dist
import Sound.Pred.Machine
import Sound.Pred.Model

-- Подстановки ----------------------------------------------------------------

-- | Морфизм на конечном алфавите: буква в слово.
type Rules a = [(a, [a])]

-- | Неподвижная точка морфизма как бесконечное слово.
--
-- Требуется продолжимость: образ затравки обязан с неё же начинаться,
-- иначе неподвижной точки нет и ленивый узел ниже просто зациклится.
-- Проверяем явно, потому что <<loop>> в рантайме не объясняет причину.
substWord :: (Ord a) => Rules a -> a -> [a]
substWord rules seed
  | take 1 (sub seed) /= [seed] = error "hsig-pred: морфизм не продолжим с этой затравки"
  | otherwise = w
  where
    table = M.fromList rules
    sub c = M.findWithDefault [c] c table
    -- Узел: слово это своя же затравка плюс собственный образ без первой
    -- буквы. Каждая буква требует только уже вычисленный префикс.
    w = seed : drop 1 (concatMap sub w)

-- | Подстановка как модель: энтропийная скорость строго нулевая, но
-- слово непериодично, и число причинных состояний неограниченно растёт.
--
-- Первый тест на то, слышна ли вообще статистическая сложность: если это
-- звучит мёртво, дело не в неожиданности, а в отображении.
substPred :: (Ord a) => Rules a -> a -> Pred a
substPred rules seed = unfoldPred out step (substWord rules seed)
  where
    out (x : _) = dirac x
    out [] = error "hsig-pred: слово подстановки кончилось"
    step (_ : rest) _ = rest
    step [] _ = error "hsig-pred: слово подстановки кончилось"

thueMorse :: Rules Char
thueMorse = [('a', "ab"), ('b', "ba")]

periodDoubling :: Rules Char
periodDoubling = [('a', "ab"), ('b', "aa")]

fibonacci :: Rules Char
fibonacci = [('a', "ab"), ('b', "a")]

thueMorseWord :: String
thueMorseWord = substWord thueMorse 'a'

periodDoublingWord :: String
periodDoublingWord = substWord periodDoubling 'a'

fibonacciWord :: String
fibonacciWord = substWord fibonacci 'a'

-- Двухсостоятельные процессы -------------------------------------------------

-- | Два причинных состояния: свободное и вынужденное.
data TwoState = SA | SB
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Процесс even: единицы идут блоками чётной длины.
--
-- Причинных состояний два, но марковский порядок бесконечен: чтобы знать,
-- обязана ли следующая единица закрыть блок, нужна чётность всего текущего
-- пробега. Модель конечного порядка не выучит его никогда, и в этом смысл
-- пары с 'goldenMean'.
evenProcess :: Double -> Machine TwoState Int
evenProcess p =
  Machine
    { machineStart = SA
    , machineStates = [SA, SB]
    , machineOut = \s -> case s of
        SA -> dist [(0, p), (1, 1 - p)]
        SB -> dirac 1
    , -- Из SB по нулю уйти нельзя, но переход обязан быть тотальным:
      -- чужую последовательность разумнее пересинхронизировать в SA,
      -- чем уронить.
      machineStep = \s x -> case (s, x) of
        (SA, 0) -> SA
        (SA, _) -> SB
        (SB, _) -> SA
    }

-- | Процесс golden mean: нет двух нулей подряд.
--
-- Топологически та же машина, что 'evenProcess', и те же @h_mu@ с @C_mu@.
-- Но это марковская цепь порядка один, и слушатель конечного порядка
-- выучивает её точно.
goldenMean :: Double -> Machine TwoState Int
goldenMean p =
  Machine
    { machineStart = SA
    , machineStates = [SA, SB]
    , machineOut = \s -> case s of
        SA -> dist [(1, p), (0, 1 - p)]
        SB -> dirac 1
    , machineStep = \s x -> case (s, x) of
        (SA, 1) -> SA
        (SA, _) -> SB
        (SB, _) -> SA
    }
