-- | Расстояние между предиктивными моделями.
--
-- Единственная метрика в подпакете, и она выводится из определения, а не
-- назначается: состояния близки ровно настолько, насколько похоже они
-- предсказывают сейчас и после любого наблюдения.
--
-- Берётся форма с максимумом, а не со средним: она доказуемо
-- псевдометрика (максимум псевдометрик - псевдометрика, а полная вариация
-- удовлетворяет неравенству треугольника), тогда как усреднение по
-- собственной мере состояния треугольник не обязано сохранять. Липшицево
-- вложение имеет смысл только относительно настоящей метрики.
module Sound.Pred.Metric
  ( bisimDist
  , bisimDistWith
  , distMatrix
  , defaultDepth
  , defaultGamma
  ) where

import Data.Map.Strict qualified as M
import Sound.Pred.Dist
import Sound.Pred.Model

-- | Глубина развёртки по умолчанию. Стоимость растёт как размер алфавита в
-- степени глубины, поэтому четыре, а не десять.
defaultDepth :: Int
defaultDepth = 4

-- | Дисконт будущего. Половина: расхождение через шаг весит вдвое меньше
-- расхождения сейчас, как и в слуховом впечатлении.
defaultGamma :: Double
defaultGamma = 0.5

-- | Бисимуляционная псевдометрика с умолчаниями.
bisimDist :: (Ord a) => Pred a -> Pred a -> Double
bisimDist = bisimDistWith defaultDepth defaultGamma

-- | Псевдометрика с явной глубиной и дисконтом. Значения лежат в [0, 1].
--
-- Ноль означает неотличимость на данной глубине, то есть бисимуляцию.
-- Модели, построенные совсем по-разному, но ведущие себя одинаково,
-- обязаны давать ноль: в этом весь смысл коалгебраического взгляда.
bisimDistWith :: (Ord a) => Int -> Double -> Pred a -> Pred a -> Double
bisimDistWith depth gamma = go depth
  where
    go k s t
      | k <= 0 = 0
      | otherwise = max now later
      where
        now = totalVariation (predict s) (predict t)
        -- Объединение носителей, а не пересечение: иначе метрика была бы
        -- несимметричной, а различие, видимое только одной из моделей,
        -- терялось бы.
        syms = M.keys (M.fromList [(x, ()) | x <- support (predict s) <> support (predict t)])
        later = gamma * maximum (0 : [go (k - 1) (observe s x) (observe t x) | x <- syms])

-- | Матрица попарных расстояний, симметричная, с нулями на диагонали.
distMatrix :: (Ord a) => [Pred a] -> [[Double]]
distMatrix ms = [[bisimDist s t | t <- ms] | s <- ms]
