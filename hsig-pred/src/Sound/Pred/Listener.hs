-- | Слушатель: долговременная и краткосрочная модели переменного порядка.
--
-- Одновременно измерительный прибор (сюрприз, энтропия) и целевая функция
-- композитора. Обучается только на том, что уже прозвучало: заглядывание
-- вперёд замкнуло бы контур и сделало любую пьесу «выученной» (docs/PRED.md,
-- разд. 9).
--
-- Устройство из IDyOM. Долговременная цепь копит статистику всей пьесы и
-- не сбрасывается никогда. Краткосрочная сбрасывается на границе фразы и
-- потому ловит локальную повторность, которую долговременная размазывает по
-- среднему. Их предсказания сводятся взвешенным геометрическим средним, а
-- веса берутся из энтропии: уверенная компонента весит больше.
--
-- Зачем это композитору. Разбиение на фразы - единственное, чем можно
-- влиять на слушателя, ничего не меняя в самом потоке символов. Отбор
-- материала по правдоподобию смещает статистику и портит обучение (разд. 7);
-- выбор, где поставить границу, не смещает ничего.
--
-- Сглаживание внутри цепи по механизму ухода PPM-C: масса, отданная вниз,
-- равна числу различных символов, встреченных в этом контексте, делённому
-- на общее число наблюдений плюс это же число. Постоянного веса отката тут
-- нет, и это важно. Фиксированный вес приходится подбирать под длину окна:
-- большой не даёт краткосрочной цепи поверить увиденному за одну фразу,
-- малый делает её уверенной и неправой на первых же событиях, когда после
-- одного символа она ждёт его повтора с вероятностью восемь десятых.
-- У PPM-C уверенность считается сама из того, сколько разных продолжений
-- контекст уже видел.
--
-- Обученных на корпусе весов нет, всё считается на лету от нуля, иначе
-- эксперимент невоспроизводим.
module Sound.Pred.Listener
  ( Listener
  , newListener
  , newListenerWith
  , listenerOrder
  , listenerHist
  , hasShortTerm
  , predictNext
  , predictAfter
  , predictLongAfter
  , observeSym
  , boundary
  , trainOn
  , trainSegmented
  , onlineSurprisals
  , onlineEntropies
  , onlineSurprisalsSeg
  , tailMean
  ) where

import Data.Map.Strict qualified as M
import Sound.Pred.Dist

-- | Цепь переменного порядка: счётчики по контекстам всех длин до порядка
-- и недавняя история.
data Chain a = Chain
  { chainOrder :: !Int
  , chainCounts :: M.Map [a] (M.Map a Int)
  , chainHist :: [a]
  -- ^ новейший символ первым
  }

emptyChain :: Int -> Chain a
emptyChain k = Chain {chainOrder = k, chainCounts = M.empty, chainHist = []}

-- | Слушатель: долговременная цепь и, возможно, краткосрочная.
data Listener a = Listener
  { lisAlphabet :: [a]
  , lisBias :: Double
  , lisLong :: Chain a
  , lisShort :: Maybe (Chain a)
  }

-- | Слушатель только с долговременной памятью.
--
-- Умолчание намеренно без краткосрочной: на стационарном процессе она
-- ничего не добавляет, а замкнутые числа приёмочных проверок (разд. 5)
-- относятся именно к чистой цепи переменного порядка. Краткосрочная
-- включается явно, там где есть локальная структура.
newListener :: Int -> [a] -> Listener a
newListener k alphabet = newListenerWith k 0 alphabet

-- | Слушатель с заданными порядками долговременной и краткосрочной цепей.
-- Непозитивный второй порядок означает «краткосрочной нет».
newListenerWith :: Int -> Int -> [a] -> Listener a
newListenerWith k ks alphabet
  | k < 0 = error "hsig-pred: отрицательный порядок слушателя"
  | null alphabet = error "hsig-pred: пустой алфавит слушателя"
  | otherwise =
      Listener
        { lisAlphabet = alphabet
        , -- Смещение весов из IDyOM: вес компоненты падает как энтропия в
          -- этой степени. Тройка заметно предпочитает уверенную компоненту,
          -- выше семи разница уже неразличима.
          lisBias = 3
        , lisLong = emptyChain k
        , lisShort = if ks > 0 then Just (emptyChain ks) else Nothing
        }

-- | Порядок долговременной цепи.
listenerOrder :: Listener a -> Int
listenerOrder = chainOrder . lisLong

-- | Недавняя история долговременной цепи, новейший символ первым.
listenerHist :: Listener a -> [a]
listenerHist = chainHist . lisLong

-- | Есть ли краткосрочная компонента.
hasShortTerm :: Listener a -> Bool
hasShortTerm l = case lisShort l of
  Just _ -> True
  Nothing -> False

-- Предсказание ----------------------------------------------------------------

-- | Предсказание одной цепи по явному контексту.
chainPredict :: (Ord a) => Listener a -> Chain a -> [a] -> Dist a
chainPredict l c hist = dist [(x, prob (take (chainOrder c) hist) x) | x <- lisAlphabet l]
  where
    base = 1 / fromIntegral (length (lisAlphabet l))
    prob ctx x
      | n == 0 = backoff
      | otherwise = (fromIntegral cnt + distinct * backoff) / (fromIntegral n + distinct)
      where
        tbl = M.findWithDefault M.empty ctx (chainCounts c)
        cnt = M.findWithDefault 0 x tbl
        n = sum (M.elems tbl)
        distinct = fromIntegral (M.size tbl)
        -- Откат укорачивает контекст с дальнего конца: init убирает самый
        -- старый символ, потому что история хранится новейшим вперёд.
        backoff = if null ctx then base else prob (init ctx) x

-- | Предсказание после всего услышанного.
predictNext :: (Ord a) => Listener a -> Dist a
predictNext l = predictAfter l (chainHist (lisLong l))

-- | Предсказание после явно заданного контекста, новейший символ первым.
--
-- Нужно для замера ошибки модели в пробных точках: композитор обязан уметь
-- спросить «а что бы слушатель сказал вот здесь», не трогая его историю.
predictAfter :: (Ord a) => Listener a -> [a] -> Dist a
predictAfter l hist = case lisShort l of
  Nothing -> long
  Just s -> merge l [long, chainPredict l s hist]
  where
    long = chainPredict l (lisLong l) hist

-- | Предсказание одной долговременной цепи, без краткосрочной.
--
-- Это то, что слушатель унесёт с собой. Краткосрочная память стирается на
-- каждой границе фразы, поэтому мерять через неё усвоение процесса нельзя:
-- получится оценка не знания, а внутрифразовой повторности. Хуже того,
-- такая мера создаёт извращённый стимул - композитору выгодно делать такты
-- самоповторными, что снижает измеренную ошибку, ничему не уча.
predictLongAfter :: (Ord a) => Listener a -> [a] -> Dist a
predictLongAfter l = chainPredict l (lisLong l)

-- | Взвешенное геометрическое среднее предсказаний.
--
-- Вес компоненты падает с ростом её энтропии: та, что менее уверена,
-- меньше влияет. Свежесброшенная краткосрочная цепь почти равномерна и
-- потому почти не мешает, а набрав статистику внутри фразы - начинает
-- вести.
--
-- Веса обязаны быть нормированы, и это не косметика. Сырой вес растёт как
-- энтропия в минус третьей степени, то есть у уверенной компоненты
-- достигает тысяч. Произведение вероятностей в таких степенях уходит под
-- порог double, символ выпадает из носителя и получает бесконечный
-- сюрприз. При сумме весов в единицу результат по построению лежит между
-- наименьшей и наибольшей из вероятностей компонент, значит носитель полон,
-- пока полон он у каждой из них.
merge :: (Ord a) => Listener a -> [Dist a] -> Dist a
merge l ds = dist [(x, exp (logScore x - top)) | x <- lisAlphabet l]
  where
    hmax = logBase 2 (fromIntegral (length (lisAlphabet l)))
    raw d
      | hmax <= 0 = 1
      | otherwise = max 1e-3 (entropy d / hmax) ** negate (lisBias l)
    raws = map raw ds
    total = sum raws
    ws = if total > 0 then map (/ total) raws else map (const (1 / fromIntegral (length ds))) ds
    logScore x = sum [w * log (max 1e-300 (probOf x d)) | (d, w) <- zip ds ws]
    top = maximum (map logScore (lisAlphabet l))

-- Обучение ---------------------------------------------------------------------

chainObserve :: (Ord a) => a -> Chain a -> Chain a
chainObserve x c =
  c
    { chainCounts = foldl bump (chainCounts c) ctxs
    , chainHist = take (chainOrder c) (x : chainHist c)
    }
  where
    ctxs = [take j (chainHist c) | j <- [0 .. chainOrder c]]
    bump m ctx = M.insertWith (M.unionWith (+)) ctx (M.singleton x 1) m

-- | Услышать символ: обновить обе цепи.
observeSym :: (Ord a) => Listener a -> a -> Listener a
observeSym l x =
  l
    { lisLong = chainObserve x (lisLong l)
    , lisShort = fmap (chainObserve x) (lisShort l)
    }

-- | Граница фразы: краткосрочная память обнуляется, долговременная цела.
--
-- Единственная ручка композитора, не трогающая поток символов. Без
-- краткосрочной цепи это тождество, и так и задумано: включать её или нет
-- решает тот, кто собирает слушателя.
boundary :: Listener a -> Listener a
boundary l = l {lisShort = fmap (emptyChain . chainOrder) (lisShort l)}

-- | Услышать последовательность.
trainOn :: (Ord a) => Listener a -> [a] -> Listener a
trainOn = foldl observeSym

-- | Услышать последовательность фраз, обнуляя краткосрочную память между
-- ними.
trainSegmented :: (Ord a) => Listener a -> [[a]] -> Listener a
trainSegmented = foldl (\l seg -> trainOn (boundary l) seg)

-- Измерение ---------------------------------------------------------------------

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

-- | Сюрпризы по фразам: краткосрочная память обнуляется между ними.
onlineSurprisalsSeg :: (Ord a) => Listener a -> [[a]] -> [Double]
onlineSurprisalsSeg _ [] = []
onlineSurprisalsSeg l (seg : rest) = here <> onlineSurprisalsSeg l' rest
  where
    started = boundary l
    here = onlineSurprisals started seg
    l' = trainOn started seg

-- | Среднее по хвосту: отбрасывает переходный участок обучения.
tailMean :: Double -> [Double] -> Double
tailMean share xs
  | null kept = 0 / 0
  | otherwise = sum kept / fromIntegral (length kept)
  where
    n = length xs
    kept = drop (n - max 1 (round (share * fromIntegral n))) xs
