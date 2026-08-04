-- | Предиктивная модель как коалгебра.
--
-- Состояние определено ровно тем, что оно предсказывает, и ничем сверх того:
-- @Pred@ это @Cofree ((->) a) (Dist a)@. Отсюда нет ни 'Eq', ни 'Show' -
-- наблюдать модель можно только конечными префиксами, а сравнивать через
-- бисимуляционную метрику (см. @Sound.Pred.Metric@).
--
-- Это противоположность свободной алгебре: там термы строят и потом
-- интерпретируют, здесь смысл состояния и есть его поведение (docs/PRED.md,
-- разд. 2).
module Sound.Pred.Model
  ( -- * Тип
    Pred (..)
  , unfoldPred
  , constPred

    -- * Наблюдение
  , walk
  , surprisals
  , entropies
  , logLik

    -- * Генерация
  , generate
  , generateSeeded
  , uniformsFrom

    -- * Комбинаторы
  , mixture
  , mixtureWith
  , par
  , nest
  , nestWith

    -- * Умолчания прореживания
  , defaultCap
  , defaultFloor
  ) where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Sound.Pred.Dist
import Sound.Sig.Random (doubleAt)

-- | Модель: что будет дальше и во что превращается состояние после
-- наблюдения.
data Pred a = Pred
  { predict :: Dist a
  , observe :: a -> Pred a
  }

-- | Модель из системы переходов: анаморфизм.
unfoldPred :: (s -> Dist a) -> (s -> a -> s) -> s -> Pred a
unfoldPred out step = go
  where
    go s = Pred (out s) (go . step s)

-- | Независимые одинаково распределённые события: состояние одно.
constPred :: Dist a -> Pred a
constPred d = m
  where
    m = Pred d (const m)

-- Наблюдение ----------------------------------------------------------------

-- | Состояния до каждого наблюдения и после последнего: на одно больше,
-- чем наблюдений.
walk :: Pred a -> [a] -> [Pred a]
walk = scanl observe

-- | Информационное содержание каждого наблюдения в битах.
surprisals :: (Ord a) => Pred a -> [a] -> [Double]
surprisals m xs = zipWith (\s x -> surprisalOf (predict s) x) (walk m xs) xs

-- | Энтропия предсказания в момент каждого наблюдения.
entropies :: Pred a -> [a] -> [Double]
entropies m xs = zipWith (\s _ -> entropy (predict s)) (walk m xs) xs

-- | Логарифмическое правдоподобие последовательности в битах.
logLik :: (Ord a) => Pred a -> [a] -> Double
logLik m = negate . sum . surprisals m

-- Генерация ------------------------------------------------------------------

-- | След модели по потоку равномерных чисел из [0, 1).
generate :: Pred a -> [Double] -> [a]
generate _ [] = []
generate m (u : us) = x : generate (observe m x) us
  where
    x = sampleWith u (predict m)

-- | Бесконечный след от индексируемого splitmix (см. 'Sound.Sig.Random').
generateSeeded :: Int -> Pred a -> [a]
generateSeeded seed m = generate m (uniformsFrom seed)

-- | Бесконечный поток равномерных чисел; зависит только от seed.
uniformsFrom :: Int -> [Double]
uniformsFrom seed = map (doubleAt seed) [0 ..]

-- Комбинаторы ----------------------------------------------------------------

-- | Потолок числа живых компонент смеси.
defaultCap :: Int
defaultCap = 16

-- | Порог веса, ниже которого компонента считается мёртвой.
defaultFloor :: Double
defaultFloor = 1e-9

-- | Суперпозиция моделей: наблюдение переносит вес по правдоподобию.
--
-- Это не «случайный выбор одной из моделей», а держание всех сразу до
-- прихода различающих данных. Слышится как тональная неоднозначность,
-- которую разрешает одна нота.
mixture :: (Ord a) => [(Double, Pred a)] -> Pred a
mixture = mixtureWith defaultCap defaultFloor

-- | 'mixture' с явным прореживанием: не более @cap@ компонент, вес ниже
-- @floorW@ считается нулём.
mixtureWith :: (Ord a) => Int -> Double -> [(Double, Pred a)] -> Pred a
mixtureWith cap floorW = go . trimBy cap floorW . filter ((> 0) . fst)
  where
    -- Единственная выжившая компонента это она сама. Без этого случая
    -- обёртки копились бы слоями и обход состояния рос бы с длиной пьесы.
    go [(_, m)] = m
    go [] = error "hsig-pred: пустая смесь"
    go cs =
      Pred
        { predict = mix [(w, predict m) | (w, m) <- cs]
        , observe = \x ->
            let ws = bayes [(w, probOf x (predict m)) | (w, m) <- cs]
                next = [(w, observe m x) | (w, (_, m)) <- zip ws cs, w > 0]
             in go (trimBy cap floorW next)
        }

-- | Независимые голоса: совместное распределение факторизуется.
par :: (Ord a, Ord b) => Pred a -> Pred b -> Pred (a, b)
par p q =
  Pred
    { predict =
        dist
          [ ((x, y), px * py)
          | (x, px) <- distPairs (predict p)
          , (y, py) <- distPairs (predict q)
          ]
    , observe = \(x, y) -> par (observe p x) (observe q y)
    }

-- | Иерархия: символ верхнего уровня разворачивается в блок из @n@ событий
-- нижнего.
--
-- Верхний символ латентный, наружу видны только события нижнего уровня, и
-- вывод о том, в каком блоке мы находимся, делает тот же байес. Это
-- источник корреляций на нескольких масштабах: плоская цепь любого
-- конечного порядка их не даёт.
nest :: (Ord a) => Int -> Pred m -> (m -> Pred a) -> Pred a
nest = nestWith defaultCap defaultFloor

-- | 'nest' с явным прореживанием.
--
-- Ветвление хранится плоским списком, а не вложенными 'mixture': вложение
-- давало бы дерево глубиной в число блоков и обход, растущий степенью.
nestWith :: (Ord a) => Int -> Double -> Int -> Pred m -> (m -> Pred a) -> Pred a
nestWith cap floorW n upper leaf
  | n <= 0 = error "hsig-pred: nest с непозитивной длиной блока"
  | otherwise = go (expand 1 upper)
  where
    -- Ветка это текущая модель блока, верхний уровень после выбора символа
    -- и сколько событий блока осталось. Вес идёт рядом, а не внутри:
    -- прореживание общее с 'mixture'.
    expand w up = [(w * p, (leaf m, observe up m, n)) | (m, p) <- distPairs (predict up)]

    go brs =
      Pred
        { predict = mix [(w, predict lo) | (w, (lo, _, _)) <- brs]
        , observe = \x ->
            let ws = bayes [(w, probOf x (predict lo)) | (w, (lo, _, _)) <- brs]
                stepped = concat (zipWith (step x) ws brs)
             in go (trimBy cap floorW stepped)
        }

    step x w (_, (lo, up, k))
      | w <= 0 = []
      | k > 1 = [(w, (observe lo x, up, k - 1))]
      | otherwise = expand w up

-- | Оставить не более @cap@ тяжёлых веток, выбросить лёгкие, перенормировать.
--
-- Если под порог ушло всё, оставляем самую тяжёлую: пустое ветвление
-- означало бы модель без предсказания, а такой в коалгебре не бывает.
trimBy :: Int -> Double -> [(Double, b)] -> [(Double, b)]
trimBy cap floorW brs
  | null positive = []
  | otherwise = [(w / total, b) | (w, b) <- kept]
  where
    -- Фильтр по строгой положительности идёт до всего остального: он же
    -- гарантирует, что делить на total можно.
    positive = filter ((> 0) . fst) brs
    sorted = take cap (sortOn (Down . fst) positive)
    heavy = filter ((>= floorW) . fst) sorted
    kept = if null heavy then take 1 sorted else heavy
    total = sum (map fst kept)
