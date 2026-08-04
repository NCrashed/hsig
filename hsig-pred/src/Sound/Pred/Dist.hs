-- | Конечное распределение над исходами.
--
-- Инвариант: веса строго положительны, в сумме дают единицу, исходы
-- уникальны и упорядочены. Всё это устанавливается в 'dist' и держится
-- каждой операцией, поэтому сравнивать распределения можно поэлементно.
--
-- Логарифмы всюду двоичные: единица измерения бит, чтобы значения
-- инвариантов машин (docs/PRED.md, разд. 5) сверялись напрямую.
module Sound.Pred.Dist
  ( -- * Тип
    Dist
  , distPairs
  , support
  , probOf

    -- * Построение
  , dist
  , dirac
  , uniform
  , mapDist

    -- * Информация
  , entropy
  , surprisalOf
  , kl
  , totalVariation

    -- * Комбинирование
  , mix
  , bayes
  , prune

    -- * Выборка
  , sampleWith
  ) where

import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Ord (Down (..))

-- | Нормированное распределение. Конструктор скрыт: инвариант держит 'dist'.
newtype Dist a = Dist {distPairs :: [(a, Double)]}
  deriving (Eq, Show)

-- | Исходы в каноническом порядке.
support :: Dist a -> [a]
support = map fst . distPairs

-- | Вероятность исхода; вне носителя ноль.
probOf :: (Ord a) => a -> Dist a -> Double
probOf x (Dist ps) = maybe 0 id (lookup x ps)

-- | Распределение по весам. Веса произвольные неотрицательные, одинаковые
-- исходы складываются, неположительные выбрасываются.
--
-- Пустой носитель это ошибка вызывающего, а не пустое распределение:
-- предсказывать нечего означает, что модель построена неверно.
dist :: (Ord a) => [(a, Double)] -> Dist a
dist ws
  | total <= 0 = error "hsig-pred: пустое распределение"
  | otherwise = Dist [(x, w / total) | (x, w) <- kept]
  where
    kept = M.toAscList (M.filter (> 0) (M.fromListWith (+) ws))
    total = sum (map snd kept)

-- | Сосредоточенное в одной точке.
dirac :: a -> Dist a
dirac x = Dist [(x, 1)]

-- | Равномерное по списку исходов.
uniform :: (Ord a) => [a] -> Dist a
uniform xs = dist [(x, 1) | x <- xs]

-- | Образ под функцией: склеенные исходы складывают вероятности.
mapDist :: (Ord b) => (a -> b) -> Dist a -> Dist b
mapDist f (Dist ps) = dist [(f x, p) | (x, p) <- ps]

-- Информация ----------------------------------------------------------------

-- | Энтропия Шеннона в битах.
entropy :: Dist a -> Double
entropy (Dist ps) = negate (sum [p * logBase 2 p | (_, p) <- ps])

-- | Информационное содержание исхода в битах, вне носителя бесконечность.
surprisalOf :: (Ord a) => Dist a -> a -> Double
surprisalOf d x = negate (logBase 2 (probOf x d))

-- | Расхождение Кульбака-Лейблера @KL(p || q)@ в битах.
--
-- Бесконечно, если @p@ выходит за носитель @q@. Возвращаем честную
-- бесконечность, а не срезанную константу: сглаживание это дело
-- вызывающего, и молчаливая подмена спрятала бы дыру в его модели.
kl :: (Ord a) => Dist a -> Dist a -> Double
kl p q = sum [px * logBase 2 (px / probOf x q) | (x, px) <- distPairs p]

-- | Полная вариация: половина суммы модулей разностей. Лежит в [0, 1].
totalVariation :: (Ord a) => Dist a -> Dist a -> Double
totalVariation p q = 0.5 * sum [abs (probOf x p - probOf x q) | x <- keys]
  where
    keys = M.keys (M.fromList [(x, ()) | x <- support p <> support q])

-- Комбинирование ------------------------------------------------------------

-- | Взвешенная смесь. Веса произвольные неотрицательные, нормируются сами.
mix :: (Ord a) => [(Double, Dist a)] -> Dist a
mix cs = dist [(x, w * p) | (w, d) <- cs, w > 0, (x, p) <- distPairs d]

-- | Апостериорные веса по приорам и правдоподобиям наблюдения.
--
-- Наблюдение, невозможное под всеми компонентами, не различает их: делить
-- было бы не на что, и правильный предел это нетронутый приор. Возврат
-- NaN на этом месте тихо отравил бы всю дальнейшую генерацию.
bayes :: [(Double, Double)] -> [Double]
bayes cs
  | evidence > 0 = [w * l / evidence | (w, l) <- cs]
  | priors > 0 = [w / priors | (w, _) <- cs]
  | otherwise = [1 / fromIntegral (length cs) | _ <- cs]
  where
    evidence = sum [w * l | (w, l) <- cs]
    priors = sum (map fst cs)

-- | Оставить не более @k@ самых вероятных исходов и перенормировать.
--
-- Нужно там, где смесь растёт (см. 'Sound.Pred.Model.nest'): без потолка
-- число живых компонент растёт степенью глубины.
prune :: (Ord a) => Int -> Dist a -> Dist a
prune k d
  | k <= 0 = error "hsig-pred: prune с непозитивным потолком"
  | otherwise = dist (take k (sortOn (Down . snd) (distPairs d)))

-- Выборка --------------------------------------------------------------------

-- | Исход по числу из [0, 1) методом обратной функции распределения.
--
-- Значения вне отрезка прижимаются к краям, а не считаются ошибкой:
-- источник равномерных чисел бывает полуоткрыт с той или другой стороны,
-- и падать на границе хуже, чем выдать крайний исход.
sampleWith :: Double -> Dist a -> a
sampleWith u (Dist ps) = go ps (u * total)
  where
    total = sum (map snd ps)
    go [(x, _)] _ = x
    go ((x, p) : rest) acc
      | acc < p = x
      | otherwise = go rest (acc - p)
    go [] _ = error "hsig-pred: пустое распределение в выборке"
