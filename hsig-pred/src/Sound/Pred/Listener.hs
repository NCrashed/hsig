-- | Слушатель: марковская модель переменного порядка.
--
-- Одновременно измерительный прибор (сюрприз, энтропия) и целевая функция
-- композитора. Обучается только на том, что уже прозвучало: заглядывание
-- вперёд замкнуло бы контур и сделало любую пьесу «выученной» (docs/PRED.md,
-- разд. 9).
--
-- Сглаживание интерполяционное: предсказание порядка @k@ смешивается с
-- предсказанием порядка @k-1@ с весом отката, и так до равномерного. Это
-- форма из IDyOM, только без обученных на корпусе весов - всё считается на
-- лету от нуля, иначе эксперимент невоспроизводим.
module Sound.Pred.Listener
  ( Listener
  , newListener
  , listenerOrder
  , listenerHist
  , predictNext
  , predictAfter
  , observeSym
  , trainOn
  , onlineSurprisals
  , onlineEntropies
  , tailMean
  ) where

import Data.Map.Strict qualified as M
import Sound.Pred.Dist

-- | Состояние слушателя: счётчики по контекстам всех длин до порядка и
-- недавняя история.
data Listener a = Listener
  { lisOrder :: Int
  , lisAlphabet :: [a]
  , lisBackoff :: Double
  , lisCounts :: M.Map [a] (M.Map a Int)
  , lisHist :: [a]
  -- ^ новейший символ первым
  }

-- | Пустой слушатель заданного порядка над известным алфавитом.
--
-- Алфавит нужен заранее: предсказывать надо и то, чего ещё не слышал, иначе
-- первое появление символа стоило бы бесконечного сюрприза.
newListener :: Int -> [a] -> Listener a
newListener k alphabet
  | k < 0 = error "hsig-pred: отрицательный порядок слушателя"
  | null alphabet = error "hsig-pred: пустой алфавит слушателя"
  | otherwise =
      Listener
        { lisOrder = k
        , lisAlphabet = alphabet
        , lisBackoff = 1
        , lisCounts = M.empty
        , lisHist = []
        }

-- | Наибольшая длина контекста, которую слушатель различает.
listenerOrder :: Listener a -> Int
listenerOrder = lisOrder

-- | Недавняя история, новейший символ первым.
listenerHist :: Listener a -> [a]
listenerHist = lisHist

-- | Предсказание после всего услышанного.
predictNext :: (Ord a) => Listener a -> Dist a
predictNext l = predictAfter l (lisHist l)

-- | Предсказание после явно заданного контекста, новейший символ первым.
--
-- Нужно для замера ошибки модели в пробных точках: композитор обязан уметь
-- спросить «а что бы слушатель сказал вот здесь», не трогая его историю.
predictAfter :: (Ord a) => Listener a -> [a] -> Dist a
predictAfter l hist = dist [(x, prob (take (lisOrder l) hist) x) | x <- lisAlphabet l]
  where
    alpha = lisBackoff l
    base = 1 / fromIntegral (length (lisAlphabet l))
    prob ctx x = (fromIntegral c + alpha * backoff) / (fromIntegral n + alpha)
      where
        tbl = M.findWithDefault M.empty ctx (lisCounts l)
        c = M.findWithDefault 0 x tbl
        n = sum (M.elems tbl)
        -- Откат укорачивает контекст с дальнего конца: init убирает самый
        -- старый символ, потому что история хранится новейшим вперёд.
        backoff = if null ctx then base else prob (init ctx) x

-- | Услышать символ: обновить счётчики всех контекстов и историю.
observeSym :: (Ord a) => Listener a -> a -> Listener a
observeSym l x =
  l
    { lisCounts = foldl bump (lisCounts l) ctxs
    , lisHist = take (lisOrder l) (x : lisHist l)
    }
  where
    ctxs = [take j (lisHist l) | j <- [0 .. lisOrder l]]
    bump m ctx = M.insertWith (M.unionWith (+)) ctx (M.singleton x 1) m

-- | Услышать последовательность.
trainOn :: (Ord a) => Listener a -> [a] -> Listener a
trainOn = foldl observeSym

-- | Сюрприз каждого символа в битах при онлайновом обучении: сначала
-- предсказать, потом услышать.
--
-- Именно это число слышит человек, а не сюрприз под финальной моделью:
-- пьеса учит по ходу, и первые такты обязаны быть неожиданнее последних.
onlineSurprisals :: (Ord a) => Listener a -> [a] -> [Double]
onlineSurprisals _ [] = []
onlineSurprisals l (x : xs) = surprisalOf (predictNext l) x : onlineSurprisals (observeSym l x) xs

-- | Энтропия предсказания перед каждым символом при онлайновом обучении.
--
-- Это то, сколько слушатель ожидает удивиться. Разница с фактическим
-- сюрпризом и есть мера того, насколько материал выходит за его текущую
-- модель: абсолютный порог тут не годится, потому что в начале пьесы
-- слушатель не знает ничего и любой такт для него неожидан.
onlineEntropies :: (Ord a) => Listener a -> [a] -> [Double]
onlineEntropies _ [] = []
onlineEntropies l (x : xs) = entropy (predictNext l) : onlineEntropies (observeSym l x) xs

-- | Среднее по хвосту: отбрасывает переходный участок обучения.
tailMean :: Double -> [Double] -> Double
tailMean share xs
  | null kept = 0 / 0
  | otherwise = sum kept / fromIntegral (length kept)
  where
    n = length xs
    kept = drop (n - max 1 (round (share * fromIntegral n))) xs
