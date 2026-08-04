-- | Липшицева укладка состояний в произвольное пространство.
--
-- Раньше укладка умела только аккорды. Это оказалось частным случаем: есть
-- жанры, где носителем информации служит не высота, а фактура, и тогда
-- укладывать надо тембры, а метрикой брать перцептивную близость звука
-- (docs/PRED.md, этап M7).
--
-- Требование при этом не меняется ни на букву: близко предсказывающие
-- состояния обязаны звучать похоже. Меняется только то, в чём меряется
-- «похоже», и потому оно вынесено в параметр.
module Sound.Pred.Embed
  ( Space (..)
  , embedIn
  , stressIn
  , lipschitzIn
  , distortionIn
  , ratiosIn
  ) where

import Data.List (foldl')

-- | Целевое пространство укладки.
--
-- Позиция это не сам объект, а адрес в пространстве: у аккордов основание и
-- фигура, у тембра точка решётки координат. Из позиции получается объект,
-- по объектам считается расстояние.
data Space p = Space
  { spaceNeighbours :: p -> [p]
  -- ^ соседние позиции, куда поиск может шагнуть
  , spaceDist :: p -> p -> Double
  -- ^ перцептивное расстояние между позициями
  , spaceStarts :: Int -> [[p]]
  -- ^ стартовые раскладки для @n@ состояний, несколько штук
  , spaceSame :: p -> p -> Bool
  -- ^ звучат ли позиции одинаково; используется для взаимной однозначности
  }

-- | Целевые расстояния: матрица модели, растянутая до @span@.
targetsOf :: Double -> [[Double]] -> [[Double]]
targetsOf sp d = [[k * x | x <- row] | row <- d]
  where
    top = maximum (0 : concat d)
    k = if top > 0 then sp / top else 0

-- | Невязка укладки: сумма квадратов отклонений от цели.
stressIn :: Space p -> Double -> [[Double]] -> [p] -> Double
stressIn sc sp d ps =
  sum
    [ (spaceDist sc (ps !! i) (ps !! j) - tg !! i !! j) ** 2
    | i <- [0 .. n - 1]
    , j <- [i + 1 .. n - 1]
    ]
  where
    n = length ps
    tg = targetsOf sp d

-- | Уложить состояния так, чтобы близкие предсказания стали близким
-- звучанием.
--
-- Координатный спуск из нескольких детерминированных стартов. Случайности
-- нет: воспроизводимость дороже оптимума, а качество укладки меряется
-- отдельно ('lipschitzIn', 'distortionIn'), а не предполагается.
--
-- Два состояния не могут занять одинаково звучащую позицию. Это не вкус, а
-- условие осмысленности: позиция объявлена носителем причинного состояния,
-- и совпадение означает, что состояние ею не кодируется. Стресс такое
-- допускает охотно, поэтому запрет стоит в поиске, а не в оценке.
embedIn :: Space p -> Double -> Int -> [[Double]] -> [p]
embedIn sc sp rounds d = lowest [go rounds s | s <- spaceStarts sc n]
  where
    n = length d
    at = stressIn sc sp d
    lowest = foldr1 (\a b -> if at a <= at b then a else b)

    go 0 ps = ps
    go r ps
      | at ps' < at ps - 1e-12 = go (r - 1) ps'
      | otherwise = ps
      where
        ps' = sweep ps

    sweep ps = foldl' improve ps [0 .. n - 1]

    improve ps i = pick ps [replaceAt i q ps | q <- spaceNeighbours sc (ps !! i), free ps i q]
      where
        pick acc [] = acc
        pick acc (x : xs)
          | at x < at acc = pick x xs
          | otherwise = pick acc xs

    free ps i q = all (\j -> not (spaceSame sc (ps !! j) q)) [j | j <- [0 .. n - 1], j /= i]

    replaceAt i x xs = take i xs <> [x] <> drop (i + 1) xs

-- | Отношения достигнутого расстояния к расстоянию модели по всем парам,
-- где модель их различает.
ratiosIn :: Space p -> [p] -> [[Double]] -> [Double]
ratiosIn sc ps d =
  [ spaceDist sc (ps !! i) (ps !! j) / (d !! i !! j)
  | i <- [0 .. n - 1]
  , j <- [i + 1 .. n - 1]
  , d !! i !! j > 0
  ]
  where
    n = length ps

-- | Фактическая липшицева константа укладки.
lipschitzIn :: Space p -> [p] -> [[Double]] -> Double
lipschitzIn sc ps d = maximum (0 : ratiosIn sc ps d)

-- | Билипшицево искажение: наибольшее растяжение к наименьшему.
--
-- Единица означает подобие. Бесконечность означает, что два различимых
-- состояния сели в одну позицию, то есть укладка перестала кодировать.
distortionIn :: Space p -> [p] -> [[Double]] -> Double
distortionIn sc ps d
  | null rs = 1
  | lo <= 0 = 1 / 0
  | otherwise = maximum rs / lo
  where
    rs = ratiosIn sc ps d
    lo = minimum rs
