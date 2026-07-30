-- | Партитура: алгебра паттернов и ноты.
--
-- Семантика взята из Tidal один в один (разд. 7 дизайна), но без самого
-- пакета: он тянет hosc, network и реалтайм-обвязку, которые тут не нужны.
-- Паттерн это функция из отрезка времени в события; время в циклах и
-- рациональное, чтобы деления были точными.
--
-- Один цикл это одна секунда. Темп задаётся не отдельным полем, а самим
-- паттерном: @fast 2@ это два цикла в секунду.
module Sound.Sig.Score
  ( -- * Время и события
    Time
  , Arc (..)
  , Event (..)
  , Pattern (..)

    -- * Построение
  , silence
  , fromList
  , listToPat

    -- * Комбинаторы
  , stack
  , cat
  , fastcat
  , fast
  , slow
  , rotL
  , rotR
  , every
  , rev
  , degradeBy
  , degradeSeeded
  , undegradeBy
  , undegradeSeeded
  , sometimesBy
  , sometimes
  , often
  , rarely
  , almostNever
  , almostAlways
  , superimpose
  , off
  , ply
  , iter
  , palindrome
  , whenmod
  , euclid
  , euclidInv
  , struct
  , segment
  , appLeft
  , euclidOff
  , fastGap
  , timecat
  , within
  , inside
  , swingBy
  , playWhen
  , zoom
  , linger
  , trunc
  , chunk
  , rot
  , run
  , someCyclesBy
  , randcat
  , wrandcat
  , shuffle
  , scramble

    -- * Мини-нотация
  , parsePat
  , numbers

    -- * Ноты
  , Note (..)
  , Instrument
  , noteOf
  , notes
  , noteHzOf
  , noteHz
  ) where

import Data.Char (isDigit, isSpace, toLower)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ratio (denominator, numerator, (%))
import Data.String (IsString (..))
import Sound.Sig.Core (Sig)
import Sound.Sig.Random (doubleAt)

-- | Время в циклах. Рациональное: деления обязаны быть точными.
type Time = Rational

data Arc = Arc
  { arcStart :: !Time
  , arcStop :: !Time
  }
  deriving (Eq, Ord, Show)

-- | Целый отрезок события (когда нота началась и сколько длится) и видимая
-- часть (что попало в запрос). Nothing в целом означает аналоговое
-- событие, у нас таких нет.
data Event a = Event
  { eventWhole :: !(Maybe Arc)
  , eventPart :: !Arc
  , eventValue :: a
  }
  deriving (Eq, Show)

instance Functor Event where
  fmap f e = e {eventValue = f (eventValue e)}

newtype Pattern a = Pattern {queryArc :: Arc -> [Event a]}

-- Время --------------------------------------------------------------------

-- | Начало цикла, содержащего момент.
sam :: Time -> Time
sam = fromIntegral . (floor :: Time -> Int)

wholeCycle :: Time -> Arc
wholeCycle t = Arc (sam t) (sam t + 1)

-- | Разбивает отрезок по границам циклов.
spanCycles :: Arc -> [Arc]
spanCycles (Arc s e)
  | s >= e = []
  | sam s == sam e = [Arc s e]
  | otherwise = Arc s next : spanCycles (Arc next e)
  where
    next = sam s + 1

sect :: Arc -> Arc -> Arc
sect (Arc s e) (Arc s' e') = Arc (max s s') (min e e')

-- | Пересечение, если оно непусто.
subArc :: Arc -> Arc -> Maybe Arc
subArc a b
  | arcStart c < arcStop c = Just c
  | otherwise = Nothing
  where
    c = sect a b

mapArc :: (Time -> Time) -> Arc -> Arc
mapArc f (Arc s e) = Arc (f s) (f e)

mapEvent :: (Time -> Time) -> Event a -> Event a
mapEvent f e =
  e
    { eventWhole = mapArc f <$> eventWhole e
    , eventPart = mapArc f (eventPart e)
    }

withResultTime :: (Time -> Time) -> Pattern a -> Pattern a
withResultTime f p = Pattern $ \a -> map (mapEvent f) (queryArc p a)

withQueryTime :: (Time -> Time) -> Pattern a -> Pattern a
withQueryTime f p = Pattern $ \a -> queryArc p (mapArc f a)

-- | Разбивает запрос по циклам: нужно всем, кто смотрит на номер цикла.
splitQueries :: Pattern a -> Pattern a
splitQueries p = Pattern $ concatMap (queryArc p) . spanCycles

-- Инстансы -----------------------------------------------------------------

instance Functor Pattern where
  fmap f p = Pattern $ \a -> map (fmap f) (queryArc p a)

instance Applicative Pattern where
  -- Одно событие на цикл, целый отрезок это сам цикл.
  pure v = Pattern $ \a -> map (\part -> Event (Just (wholeCycle (arcStart part))) part v) (spanCycles a)

  -- Структура берётся от обоих: части пересекаются, целые тоже.
  pf <*> px = Pattern $ \a ->
    [ Event (sect <$> eventWhole ef <*> eventWhole ex) part (eventValue ef (eventValue ex))
    | ef <- queryArc pf a
    , ex <- queryArc px a
    , Just part <- [subArc (eventPart ef) (eventPart ex)]
    ]

instance Monad Pattern where
  p >>= f = unwrap (fmap f p)

-- | Соединение по Tidal: внутренний паттерн спрашивается в абсолютном
-- времени на части внешнего события, отрезки пересекаются.
unwrap :: Pattern (Pattern a) -> Pattern a
unwrap pp = Pattern $ \a -> concatMap expand (queryArc pp a)
  where
    expand outer = mapMaybe (munge outer) (queryArc (eventValue outer) (eventPart outer))
    munge outer inner = do
      part <- subArc (eventPart outer) (eventPart inner)
      pure (Event (sect <$> eventWhole outer <*> eventWhole inner) part (eventValue inner))

-- Построение ---------------------------------------------------------------

silence :: Pattern a
silence = Pattern (const [])

-- | Список по элементу на цикл, как fromList в Tidal.
fromList :: [a] -> Pattern a
fromList = cat . map pure

-- | Весь список внутри одного цикла, как listToPat в Tidal. Имена важны:
-- при переносе на настоящий Tidal перепутанные местами семантики тихо
-- поменяли бы ритм, не сломав компиляцию.
listToPat :: [a] -> Pattern a
listToPat = fastcat . map pure

-- Комбинаторы --------------------------------------------------------------

-- | Всё одновременно.
stack :: [Pattern a] -> Pattern a
stack ps = Pattern $ \a -> concatMap (`queryArc` a) ps

-- | По паттерну на цикл.
cat :: [Pattern a] -> Pattern a
cat [] = silence
cat ps = splitQueries $ Pattern $ \a -> queryArc (shifted a) (mapArc (subtract (offset a)) a)
  where
    n = length ps
    cycleOf a = floor (arcStart a) :: Int
    index a = cycleOf a `mod` n
    offset a = fromIntegral (cycleOf a - ((cycleOf a - index a) `div` n))
    shifted a = withResultTime (+ offset a) (ps !! index a)

-- | Все подряд внутри одного цикла.
fastcat :: [Pattern a] -> Pattern a
fastcat [] = silence
fastcat ps = fast (fromIntegral (length ps)) (cat ps)

-- | Сжать по времени: fast 2 это вдвое быстрее.
fast :: Time -> Pattern a -> Pattern a
fast n p
  | n == 0 = silence
  | n < 0 = rev (fast (negate n) p)
  | otherwise = withResultTime (/ n) (withQueryTime (* n) p)

slow :: Time -> Pattern a -> Pattern a
slow n p
  | n == 0 = silence
  | otherwise = fast (recip n) p

-- | Сдвиг влево по времени.
rotL :: Time -> Pattern a -> Pattern a
rotL t p = withResultTime (subtract t) (withQueryTime (+ t) p)

rotR :: Time -> Pattern a -> Pattern a
rotR t = rotL (negate t)

-- | Аппликация со структурой левого (в Tidal это @\<*@).
--
-- Обычный '<*>' пересекает структуру обоих, поэтому частый правый дробит
-- редкий левый. Здесь части и целые берутся у левого, а у правого только
-- спрашивается значение на отрезке левого события. На этом стоят 'struct'
-- и 'segment': ритм от одного, значения от другого.
appLeft :: Pattern (a -> b) -> Pattern a -> Pattern b
appLeft pf px = Pattern $ \a ->
  [ Event (eventWhole ef) part (eventValue ef (eventValue ex))
  | ef <- queryArc pf a
  , ex <- queryArc px (fromMaybe (eventPart ef) (eventWhole ef))
  , Just part <- [subArc (eventPart ef) (eventPart ex)]
  ]

-- | Выбрасывает события без значения.
filterJust :: Pattern (Maybe a) -> Pattern a
filterJust p = Pattern $ \a -> [e {eventValue = v} | e <- queryArc p a, Just v <- [eventValue e]]

-- | Ритм от булева паттерна, значения от второго: @struct "t f t t" p@.
struct :: Pattern Bool -> Pattern a -> Pattern a
struct bp vp = filterJust (appLeft ((\b v -> if b then Just v else Nothing) <$> bp) vp)

-- | Нарезает цикл на n равных долей и берёт значение на каждой.
segment :: Time -> Pattern a -> Pattern a
segment n = appLeft (fast n (pure id))

-- | Евклидов ритм: k ударов, максимально равномерно размазанных по n шагам.
--
-- Тот самый (3,8) это трезильо, (5,8) кубинская синкопа, (2,5) хабанера.
-- Рисунок считается алгоритмом Бьорклунда, как в Tidal.
euclid :: Int -> Int -> Pattern a -> Pattern a
euclid k n = struct (listToPat (bjorklund k n))

-- | Дополнение 'euclid': удары там, где у него паузы.
euclidInv :: Int -> Int -> Pattern a -> Pattern a
euclidInv k n = struct (listToPat (map not (bjorklund k n)))

-- | Алгоритм Бьорклунда: k единиц и n-k нулей, разложенные максимально
-- равномерно. Реализация через слияние остатка, как в оригинальной статье.
bjorklund :: Int -> Int -> [Bool]
bjorklund k n
  | n <= 0 = []
  | k <= 0 = replicate n False
  | k >= n = replicate n True
  | otherwise = concat (go (replicate k [True]) (replicate (n - k) [False]))
  where
    go as bs
      | length bs <= 1 = as <> bs
      | length as <= length bs =
          let (paired, rest) = splitAt (length as) bs
           in go (zipWith (<>) as paired) rest
      | otherwise =
          let (paired, rest) = splitAt (length bs) as
           in go (zipWith (<>) paired bs) rest

-- | Евклид с поворотом рисунка на r шагов влево.
euclidOff :: Int -> Int -> Int -> Pattern a -> Pattern a
euclidOff k n r = struct (listToPat (rotate (bjorklund k n)))
  where
    rotate xs
      | null xs = xs
      | otherwise = let m = r `mod` length xs in drop m xs <> take m xs

-- | Ускоряет паттерн, не повторяя его: хвост цикла остаётся пустым.
--
-- Отличие от 'fast' принципиальное: @fast 2@ играет паттерн дважды за цикл,
-- @fastGap 2@ - один раз в первой половине. На этом стоит укладка паттерна в
-- долю цикла, то есть 'timecat' и полиметр.
fastGap :: Time -> Pattern a -> Pattern a
fastGap r p
  | r <= 0 = silence
  | otherwise = splitQueries (Pattern go)
  where
    r' = max r 1
    go a = mapMaybe back (queryArc p inner)
      where
        s0 = sam (arcStart a)
        into t = s0 + min 1 (r' * (t - s0))
        outOf t = s0 + (t - s0) / r'
        inner = Arc (into (arcStart a)) (into (arcStop a))
        back e = do
          part <- subArc (mapArc outOf (eventPart e)) a
          pure (Event (mapArc outOf <$> eventWhole e) part (eventValue e))

-- | Укладывает паттерн в отрезок цикла.
compressArc :: Arc -> Pattern a -> Pattern a
compressArc (Arc s e) p
  | s < 0 || e > 1 || s >= e = silence
  | otherwise = rotR s (fastGap (recip (e - s)) p)

-- | Последовательность с весами: доля цикла у каждого пропорциональна весу.
-- При равных весах это ровно 'fastcat'.
timecat :: [(Time, Pattern a)] -> Pattern a
timecat tps
  | total <= 0 = silence
  | otherwise = stack (go 0 tps)
  where
    total = sum (map fst tps)
    go _ [] = []
    go t ((w, p) : rest) = compressArc (Arc (t / total) ((t + w) / total)) p : go (t + w) rest

-- | Применяет функцию к позиции внутри цикла, оставляя номер цикла на месте.
mapCycle :: (Time -> Time) -> Arc -> Arc
mapCycle f (Arc s e) = Arc (s0 + f (s - s0)) (s0 + f (e - s0))
  where
    s0 = sam s

-- | Растягивает кусок цикла на весь цикл: @zoom (0, 0.25) p@ это первая
-- четверть, занявшая всё время.
zoom :: (Time, Time) -> Pattern a -> Pattern a
zoom (s, e) p
  | d <= 0 = silence
  | otherwise =
      splitQueries $
        Pattern $ \a ->
          map (mapEventArcs (mapCycle ((/ d) . subtract s))) (queryArc p (mapCycle ((+ s) . (* d)) a))
  where
    d = e - s

mapEventArcs :: (Arc -> Arc) -> Event a -> Event a
mapEventArcs f ev' = ev' {eventWhole = f <$> eventWhole ev', eventPart = f (eventPart ev')}

-- | Зацикливает первую долю цикла на весь цикл: @linger 0.25@ повторяет
-- первую четверть четыре раза. Типовой приём для заедающей пластинки.
linger :: Time -> Pattern a -> Pattern a
linger t p
  | t <= 0 = silence
  | otherwise = fast (recip t) (zoom (0, t) p)

-- | Играет только начало цикла, остальное молчит.
trunc :: Time -> Pattern a -> Pattern a
trunc t p
  | t <= 0 = silence
  | t >= 1 = p
  | otherwise = compressArc (Arc 0 t) (zoom (0, t) p)

-- | Применяет функцию к своей n-й части цикла, и на каждом цикле к
-- следующей: обработка ползёт по такту.
chunk :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
chunk n f p
  | n <= 0 = p
  | otherwise = cat [within (i % fromIntegral n, (i + 1) % fromIntegral n) f p | i <- [0 .. fromIntegral n - 1]]

-- | Сдвигает значения по событиям, не трогая ритм: рисунок тот же, ноты
-- переехали.
rot :: Int -> Pattern a -> Pattern a
rot n p = splitQueries (Pattern go)
  where
    go a = mapMaybe (clip a) (zipWith swap' events (drop k (cycle values)))
      where
        events = sortOn (arcStart . eventPart) (queryArc p (Arc (sam (arcStart a)) (sam (arcStart a) + 1)))
        values = map eventValue events
        k = if null values then 0 else n `mod` length values
        swap' e v = e {eventValue = v}
        clip q e = do
          part <- subArc (eventPart e) q
          pure e {eventPart = part}

-- | Числа от нуля до n-1 внутри цикла.
run :: Int -> Pattern Int
run n = listToPat [0 .. n - 1]

-- | Применяет функцию на случайной доле циклов. Случайность привязана к
-- номеру цикла и своему seed, поэтому рендер остаётся воспроизводимым, а
-- соседние комбинаторы не решают синхронно.
someCyclesBy :: Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
someCyclesBy amount f p = splitQueries $ Pattern $ \a ->
  let c = floor (arcStart a) :: Int
   in queryArc (if doubleAt 7 c < amount then f p else p) a

-- | Случайный паттерн из списка на каждый цикл.
randcat :: [Pattern a] -> Pattern a
randcat [] = silence
randcat ps = splitQueries $ Pattern $ \a ->
  let c = floor (arcStart a) :: Int
      i = floor (doubleAt 11 c * fromIntegral (length ps)) `mod` length ps
   in queryArc (ps !! i) a

-- | То же с весами: чем больше вес, тем чаще выпадает паттерн.
wrandcat :: [(Double, Pattern a)] -> Pattern a
wrandcat [] = silence
wrandcat wps
  | total <= 0 = silence
  | otherwise = splitQueries $ Pattern $ \a ->
      let c = floor (arcStart a) :: Int
       in queryArc (pick (doubleAt 13 c * total) wps) a
  where
    total = sum (map fst wps)
    pick _ [(_, p)] = p
    pick x ((w, p) : rest)
      | x < w = p
      | otherwise = pick (x - w) rest
    pick _ [] = silence

-- | Делит цикл на n частей и играет их в случайном порядке, с повторами.
scramble :: Int -> Pattern a -> Pattern a
scramble n p
  | n <= 0 = p
  | otherwise = splitQueries $ Pattern $ \a ->
      let c = floor (arcStart a) :: Int
          pickAt j = floor (doubleAt 17 (c * n + j) * fromIntegral n) `mod` n
       in queryArc (fastcat [slice n p (pickAt j) | j <- [0 .. n - 1]]) a

-- | То же, но перестановкой: каждая часть звучит ровно один раз.
shuffle :: Int -> Pattern a -> Pattern a
shuffle n p
  | n <= 0 = p
  | otherwise = splitQueries $ Pattern $ \a ->
      let c = floor (arcStart a) :: Int
       in queryArc (fastcat [slice n p i | i <- permutation n c]) a

-- | Кусок цикла номер i из n, растянутый на цикл.
slice :: Int -> Pattern a -> Int -> Pattern a
slice n p i = zoom (fromIntegral i % fromIntegral n, fromIntegral (i + 1) % fromIntegral n) p

-- | Детерминированная перестановка n элементов по номеру цикла: тасование
-- Фишера-Йетса на том же генераторе, что и весь шум.
permutation :: Int -> Int -> [Int]
permutation n c = go [0 .. n - 1] 0
  where
    go [] _ = []
    go xs k = case splitAt i xs of
      (before, x : after) -> x : go (before <> after) (k + 1)
      -- Недостижимо: i всегда меньше длины, но пусть падение будет внятным.
      _ -> bad "перестановка вышла за список"
      where
        i = floor (doubleAt 19 (c * n + k) * fromIntegral (length xs)) `mod` length xs

-- | Оставляет события, начало которых проходит проверку.
playWhen :: (Time -> Bool) -> Pattern a -> Pattern a
playWhen test p = Pattern $ \a -> filter ok (queryArc p a)
  where
    ok e = test (arcStart (fromMaybe (eventPart e) (eventWhole e)))

-- | Применяет функцию только к части цикла, остальное оставляет как есть.
within :: (Time, Time) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
within (s, e) f p = stack [playWhen inside' (f p), playWhen (not . inside') p]
  where
    inside' t = let pos = t - sam t in pos >= s && pos < e

-- | Смотрит на паттерн так, будто цикл в n раз короче: @inside 4 rev@
-- переворачивает каждую четверть, а не весь цикл.
inside :: Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
inside n f p = fast n (f (slow n p))

-- | Свинг: каждая вторая доля из n опаздывает на долю x от своего шага.
--
-- Ровная сетка звучит машинно, и лечится это не заменой нот, а сдвигом
-- слабых долей. Классический джазовый свинг это @swingBy (1\/3) 4@.
swingBy :: Time -> Time -> Pattern a -> Pattern a
swingBy x n = inside n (within (0.5, 1) (rotR x))

-- | Накладывает обработанную копию поверх исходного паттерна.
superimpose :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
superimpose f p = stack [p, f p]

-- | Накладывает копию, сдвинутую вперёд на долю цикла и обработанную:
-- отсюда берутся эхо, задержанные октавы и переклички.
off :: Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
off t f = superimpose (f . rotR t)

-- | Дополнение 'degradeBy': оставляет ровно те события, которые тот
-- выбрасывает. Вместе они дают исходный паттерн.
undegradeBy :: Double -> Pattern a -> Pattern a
undegradeBy = undegradeSeeded 0

-- | То же со своим потоком случайности.
undegradeSeeded :: Int -> Double -> Pattern a -> Pattern a
undegradeSeeded seed amount p = Pattern $ \a -> filter (not . keeps seed amount) (queryArc p a)

-- | Обрабатывает случайную долю событий, оставляя остальные нетронутыми.
--
-- Доля та же, что выбрасывает 'degradeBy' с тем же аргументом, поэтому
-- событий не теряется и не прибавляется.
sometimesBy :: Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
sometimesBy amount f p = stack [degradeBy amount p, f (undegradeBy amount p)]

-- | Половина событий.
sometimes :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
sometimes = sometimesBy 0.5

-- | Три четверти событий.
often :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
often = sometimesBy 0.75

-- | Четверть событий.
rarely :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
rarely = sometimesBy 0.25

-- | Каждое десятое событие.
almostNever :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
almostNever = sometimesBy 0.1

-- | Девять из десяти.
almostAlways :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
almostAlways = sometimesBy 0.9

-- | Повторяет каждое событие n раз внутри его собственного отрезка.
ply :: Time -> Pattern a -> Pattern a
ply n p = Pattern $ \a -> concatMap (expand a) (queryArc p a)
  where
    expand a e = case eventWhole e of
      Nothing -> [e]
      Just (Arc ws we) ->
        [ Event (Just (Arc s t)) part (eventValue e)
        | i <- [0 .. ceiling (max 1 n) - 1 :: Int]
        , let s = ws + fromIntegral i * step
        , let t = s + step
        , s < we
        , Just part <- [subArc (Arc s t) a]
        ]
        where
          step = (we - ws) / max 1 n

-- | Сдвигает начало паттерна на 1\/n цикла с каждым следующим циклом.
iter :: Int -> Pattern a -> Pattern a
iter n p
  | n <= 0 = p
  | otherwise = splitQueries $ Pattern $ \a ->
      let cyc = floor (arcStart a) `mod` n
       in queryArc (rotL (fromIntegral cyc % fromIntegral n) p) a

-- | Каждый второй цикл играется задом наперёд.
palindrome :: Pattern a -> Pattern a
palindrome p = cat [p, rev p]

-- | Применяет функцию, когда номер цикла по модулю n не меньше k.
whenmod :: Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
whenmod n k f p
  | n <= 0 = p
  | otherwise = splitQueries $ Pattern $ \a ->
      if floor (arcStart a) `mod` n >= k
        then queryArc (f p) a
        else queryArc p a

-- | Применяет функцию на каждом n-м цикле, начиная с нулевого.
every :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
every n f p
  | n <= 0 = p
  | otherwise = splitQueries $ Pattern $ \a ->
      if floor (arcStart a) `mod` n == (0 :: Int)
        then queryArc (f p) a
        else queryArc p a

-- | Переворачивает время внутри каждого цикла.
rev :: Pattern a -> Pattern a
rev p = splitQueries $ Pattern $ \a ->
  let c = sam (arcStart a)
      reflect = mapArc (\t -> c + c + 1 - t)
      -- Отражение меняет местами концы отрезка, поэтому нормализуем.
      flipArc (Arc s e) = Arc (min s e) (max s e)
      fixEvent e =
        e
          { eventWhole = flipArc . reflect <$> eventWhole e
          , eventPart = flipArc (reflect (eventPart e))
          }
   in map fixEvent (queryArc p (flipArc (reflect a)))

-- | Выкидывает события псевдослучайно: 0 не трогает, 1 выкидывает всё.
--
-- Значение берётся от середины целого отрезка события, а не от видимой
-- части: иначе решение зависело бы от того, как запрос разбит на блоки, и
-- одна и та же нота то звучала бы, то нет. В Tidal это тот же принцип,
-- там rand спрашивается на wholeOrPart события.
degradeBy :: Double -> Pattern a -> Pattern a
degradeBy = degradeSeeded 0

-- | То же, но со своим потоком случайности.
--
-- Нужен мини-нотации: у каждого @?@ поток обязан быть свой, иначе два
-- прореживания на одной сетке решают одинаково и слои пропадают в такт. В
-- Tidal ровно так же, там парсер выдаёт каждому @?@ отдельный seed.
degradeSeeded :: Int -> Double -> Pattern a -> Pattern a
degradeSeeded seed amount p = Pattern $ \a -> filter (keeps seed amount) (queryArc p a)

-- | Общий предикат для degrade и его дополнения: случайное число одно и то
-- же, поэтому вместе они дают ровно исходный паттерн.
keeps :: Int -> Double -> Event a -> Bool
keeps seed amount e = randomAt (fromMaybe (eventPart e) (eventWhole e)) >= amount
  where
    randomAt (Arc s t) = doubleAt seed (hashTime ((s + t) / 2))
    hashTime t = fromIntegral (numerator t) * 2654435761 + fromIntegral (denominator t)

-- Мини-нотация --------------------------------------------------------------

-- | Строка это паттерн: @"bd*4"@, @"bd ~ sn ~"@, @"[bd sn] cp"@.
instance IsString (Pattern String) where
  fromString = parsePat

-- | Разбор мини-нотации Tidal.
--
-- Поддержано: последовательность делит цикл, @~@ пауза, @*n@ ускорение,
-- @\/n@ замедление, @[..]@ подгруппа в один слот, @\<..\>@ смена по циклам,
-- запятая наложение, @?@ прореживание вдвое, @?0.3@ прореживание с явной
-- долей (точка обязательна, как в Tidal). У каждого @?@ свой поток
-- случайности, номер по порядку вхождения.
--
-- Дальше вторая партия синтаксиса: @!n@ повторяет слот, одинокий @!@
-- повторяет предыдущий, @\@n@ задаёт вес слота, @(k,n)@ и @(k,n,поворот)@
-- это евклидов ритм, точка отдельным словом группирует (@"bd . sn sn"@ это
-- @"bd [sn sn]"@), @{a b, c d e}%n@ полиметр (шаг берётся у первого слоя,
-- если нет @%@), @0 .. 3@ диапазон.
--
-- Чего пока нет: аккорды (@c'maj@) и паттерны в аргументах евклида.
parsePat :: String -> Pattern String
parsePat src = case parseStack (tokenize src) of
  (p, []) -> p
  (_, rest) -> bad ("лишнее в конце: " <> show rest)

-- | Мини-нотация с числовыми атомами.
numbers :: String -> Pattern Double
numbers = fmap toNum . parsePat
  where
    toNum w = case reads w of
      [(v, "")] -> v
      _ -> bad ("не число: " <> w)

bad :: String -> a
bad why = error ("hsig: мини-нотация, " <> why)

data Tok
  = TWord String
  | TRest
  | TOpen
  | TClose
  | TAngle
  | TUnangle
  | TComma
  | TStar
  | TSlash
  | TBang
  | TAt
  | TDot
  | TRange
  | TLParen
  | TRParen
  | TBrace
  | TUnbrace
  | TPercent
  | -- | номер потока случайности (см. 'numberQuests') и доля прореживания
    TQuest Int Double
  deriving (Eq, Show)

tokenize :: String -> [Tok]
tokenize = numberQuests . go
  where
    go [] = []
    go ('?' : cs) = quest cs
    go (c : cs)
      | isSpace c = go cs
      | Just t <- lookup c punctuation = t : go cs
      | otherwise = let (w, rest) = span plain (c : cs) in word w : go rest
    plain ch = ch /= '?' && not (isSpace ch) && ch `notElem` map fst punctuation

    -- Точка отдельным словом это группировка, две точки - диапазон. Внутри
    -- слова точка остаётся частью числа: "0.3" и "*1.5" не должны рассыпаться.
    word "." = TDot
    word ".." = TRange
    word w = TWord w

    -- Доля разбирается здесь, а не в парсере: только тут ещё видно, прижата
    -- ли она к вопросу. Пробелы токенизатор съедает, и "bd? 0.3" в парсере
    -- уже не отличить от "bd?0.3", а это разные вещи - прореживание с долей
    -- против прореживания и отдельного атома.
    --
    -- Точка в доле обязательна, как в Tidal: там долю читает Parsec-овский
    -- float, который целое число не принимает. Поэтому "bd?0" это
    -- прореживание вполовину и отдельный атом "0", а не доля ноль.
    quest cs = case span (\ch -> isDigit ch || ch == '.') cs of
      (num, rest)
        | '.' `elem` num -> case decimal num of
            Just v
              | v >= 0 && v <= 1 -> TQuest 0 (fromRational v) : go rest
              | otherwise -> bad ("доля после ? должна быть от 0 до 1: " <> num)
            Nothing -> bad ("после ? нужна доля вида 0.3: " <> num)
        | otherwise -> TQuest 0 0.5 : go (num <> rest)

-- | Нумерует вопросы слева направо, как это делает парсер Tidal.
--
-- Номер обязан зависеть только от того, сколько вопросов стоит левее. Возьми
-- мы позицию в потоке токенов, и правка соседнего слоя или лишние скобки
-- перерандомизировали бы уже подобранный грув.
numberQuests :: [Tok] -> [Tok]
numberQuests = go 0
  where
    go _ [] = []
    go !k (TQuest _ amt : ts) = TQuest k amt : go (k + 1) ts
    go !k (t : ts) = t : go k ts

punctuation :: [(Char, Tok)]
punctuation =
  [ ('~', TRest)
  , ('[', TOpen)
  , (']', TClose)
  , ('<', TAngle)
  , ('>', TUnangle)
  , (',', TComma)
  , ('*', TStar)
  , ('/', TSlash)
  , ('!', TBang)
  , ('@', TAt)
  , ('(', TLParen)
  , (')', TRParen)
  , ('{', TBrace)
  , ('}', TUnbrace)
  , ('%', TPercent)
  ]

-- | Слои через запятую.
parseStack :: [Tok] -> (Pattern String, [Tok])
parseStack ts = case parseSeq ts of
  (p, TComma : more) -> let (q, rest) = parseStack more in (stack [p, q], rest)
  (p, rest) -> (p, rest)

-- | Элемент последовательности: вес доли и сам паттерн.
data Item = Item !Time (Pattern String)

-- | Последовательность делит цикл между своими элементами.
parseSeq :: [Tok] -> (Pattern String, [Tok])
parseSeq ts = let (items, rest) = seqItems ts in (assemble items, rest)

-- | Собирает элементы в паттерн. При равных весах это ровно fastcat: путь
-- через timecat дороже и не нужен, пока никто не просил @.
assemble :: [Item] -> Pattern String
assemble [] = silence
assemble [Item _ p] = p
assemble items
  | all (\(Item w _) -> w == 1) items = fastcat [p | Item _ p <- items]
  | otherwise = timecat [(w, p) | Item w p <- items]

-- | Элементы последовательности: сюда же попадают постфиксы уровня
-- последовательности (@!@ и @\@@), группировка точками и диапазоны.
seqItems :: [Tok] -> ([Item], [Tok])
seqItems = groups [] []
  where
    -- Первый аргумент это законченные группы (в обратном порядке), второй -
    -- текущая группа (тоже в обратном).
    groups gs acc ts
      | stop ts = (finish gs (reverse acc), ts)
    groups gs acc (TDot : ts) = groups (reverse acc : gs) [] ts
    groups gs acc (TWord a : TRange : TWord b : ts)
      | Just x <- integer a
      , Just y <- integer b =
          groups gs (reverse [Item 1 (pure (show v)) | v <- range x y] <> acc) ts
    groups gs acc ts =
      let (p, rest) = parseTerm ts
       in uncurry (groups gs) (postfix (Item 1 p) acc rest)

    stop ts = case ts of
      [] -> True
      TClose : _ -> True
      TUnangle : _ -> True
      TUnbrace : _ -> True
      TComma : _ -> True
      _ -> False

    -- Точки делят последовательность на группы, каждая занимает один слот:
    -- "bd . sn sn" это то же, что "bd [sn sn]".
    finish [] cur = cur
    finish gs cur = [Item 1 (assemble g) | g <- reverse (cur : gs)]

    -- @!n@ повторяет слот n раз, одинокий @!@ - ещё раз предыдущий,
    -- @\@n@ меняет вес слота.
    postfix cur acc (TBang : TWord w : ts)
      | Just n <- integer w
      , n > 0 =
          (replicate (fromIntegral n) cur <> acc, ts)
    postfix cur acc (TBang : ts) = postfix cur (cur : acc) ts
    postfix (Item _ p) acc (TAt : TWord w : ts) = (Item (weight w) p : acc, ts)
    postfix _ _ (TAt : ts) = bad ("после @ нужно число: " <> show (take 1 ts))
    postfix cur acc ts = (cur : acc, ts)

    weight w = fromMaybe (bad ("после @ нужно число: " <> w)) (decimal w)
    range x y = if x <= y then [x .. y] else [x, x - 1 .. y]

-- | Целое из слова.
integer :: String -> Maybe Integer
integer s = case reads s of
  [(v, "")] -> Just v
  _ -> Nothing

-- | Элемент с модификаторами.
parseTerm :: [Tok] -> (Pattern String, [Tok])
parseTerm ts = mods (parseAtom ts)
  where
    mods (p, TStar : TWord w : rest) = mods (fast (rate w) p, rest)
    mods (p, TSlash : TWord w : rest) = mods (slow (rate w) p, rest)
    -- Номер потока приходит готовым из numberQuests: он равен числу
    -- вопросов левее, поэтому разные ? одной строки решают независимо.
    mods (p, TQuest k amt : rest) = mods (degradeSeeded k amt p, rest)
    -- Евклид прямо в строке: "bd(3,8)" это то же, что euclid 3 8 "bd".
    mods (p, TLParen : rest) = let (k, n, r, more) = euclidArgs rest in mods (euclidOff k n r p, more)
    mods (_, TStar : rest) = bad ("после * нужно число: " <> show (take 1 rest))
    mods (_, TSlash : rest) = bad ("после / нужно число: " <> show (take 1 rest))
    mods done = done
    rate w = fromMaybe (bad ("не число: " <> w)) (decimal w)

-- | Аргументы евклида: @(k,n)@ или @(k,n,поворот)@.
euclidArgs :: [Tok] -> (Int, Int, Int, [Tok])
euclidArgs ts = case ts of
  TWord a : TComma : TWord b : TComma : TWord c : TRParen : rest -> (whole a, whole b, whole c, rest)
  TWord a : TComma : TWord b : TRParen : rest -> (whole a, whole b, 0, rest)
  _ -> bad ("евклид ждёт (k,n) или (k,n,поворот): " <> show (take 6 ts))
  where
    whole w = maybe (bad ("не целое: " <> w)) fromIntegral (integer w)

-- | Десятичный литерал в точную дробь. Через Double он превратился бы в
-- двоичное приближение (0.1 это не 1\/10), а всё время в паттернах
-- рациональное именно ради точности делений.
decimal :: String -> Maybe Time
decimal ('-' : rest) = negate <$> decimal rest
decimal s = case span isDigit s of
  ("", _) -> Nothing
  (whole, "") -> Just (toRational (readInt whole))
  (whole, '.' : frac)
    | not (null frac) && all isDigit frac ->
        Just (toRational (readInt whole) + readInt frac % (10 ^ length frac))
  _ -> Nothing
  where
    readInt ds = read ds :: Integer

parseAtom :: [Tok] -> (Pattern String, [Tok])
parseAtom ts = case ts of
  TWord w : rest -> (pure w, rest)
  TRest : rest -> (silence, rest)
  TOpen : rest -> case parseStack rest of
    (p, TClose : more) -> (p, more)
    _ -> bad "не закрыта ["
  TAngle : rest -> case angleItems [] rest of
    (ps, TUnangle : more) -> (cat ps, more)
    _ -> bad "не закрыт <"
  -- Полиметр: у слоёв разная длина, но общий шаг. Шаг берётся у первого
  -- слоя либо задаётся через %.
  TBrace : rest -> case braceSubs [] rest of
    (subs, TUnbrace : more) ->
      let (steps, more') = case more of
            TPercent : TWord w : m -> (fromMaybe (bad ("после % нужно число: " <> w)) (decimal w), m)
            TPercent : m -> bad ("после % нужно число: " <> show (take 1 m))
            _ -> (defaultSteps subs, more)
       in (stack (map (poly steps) subs), more')
    _ -> bad "не закрыта {"
  _ -> bad ("неожидан токен: " <> show (take 1 ts))
  where
    angleItems acc rest = case rest of
      TUnangle : _ -> (reverse acc, rest)
      [] -> (reverse acc, rest)
      _ -> let (t, more) = parseTerm rest in angleItems (t : acc) more
    braceSubs acc rest =
      let (items, more) = seqItems rest
       in case more of
            TComma : after -> braceSubs (items : acc) after
            _ -> (reverse (items : acc), more)
    defaultSteps subs = case subs of
      first : _ | not (null first) -> fromIntegral (length first)
      _ -> 1
    poly steps items
      | null items = silence
      | otherwise = fast (steps / fromIntegral (length items)) (assemble items)

-- Ноты ---------------------------------------------------------------------

-- | Нота для инструмента. Момент и длительность проставляет планировщик по
-- отрезку события, в паттерне они не важны.
data Note = Note
  { noteOnset :: !Double
  -- ^ секунды от начала стема
  , noteDur :: !Double
  , noteFreq :: !Double
  , noteAmp :: !Double
  , noteLabel :: !String
  -- ^ слово из паттерна как есть: по нему набор барабанов выбирает голос,
  -- а мелодический инструмент его игнорирует
  }
  deriving (Eq, Show)

-- | Сигнал ноты начинается с нуля и конечен.
type Instrument = Note -> Sig

-- | Нота по частоте: амплитуда единичная.
noteOf :: Double -> Note
noteOf f =
  Note
    { noteOnset = 0
    , noteDur = 0
    , noteFreq = f
    , noteAmp = 1
    , noteLabel = ""
    }

-- | Частота по имени ноты: @a4@ это 440 Гц, @a1@ - 55, @c#2@ и @db2@ одно и
-- то же. Регистр не важен, диез это @#@ или @s@, бемоль @b@ или @f@,
-- октава может быть отрицательной (@c-1@).
--
-- Nothing на всём, что нотой не является: так набор барабанов и отличает
-- @bd@ от @b2@.
noteHzOf :: String -> Maybe Double
noteHzOf src = do
  (c : rest) <- pure (map toLower src)
  step <- lookup c [('c', 0), ('d', 2), ('e', 4), ('f', 5), ('g', 7), ('a', 9), ('b', 11)]
  (acc, octText) <- pure (shift rest)
  oct <- readOctave octText
  let midi = 12 * (oct + 1) + step + acc
  pure (440 * 2 ** ((fromIntegral midi - 69) / 12))
  where
    shift ('#' : r) = (1 :: Int, r)
    shift ('s' : r) = (1, r)
    shift ('b' : r) = (-1, r)
    shift ('f' : r) = (-1, r)
    shift r = (0, r)
    readOctave t = case reads t of
      [(v, "")] -> Just (v :: Int)
      _ -> Nothing

-- | Мини-нотация нотами: имя ноты даёт частоту, число берётся как частота в
-- герцах, всё прочее остаётся меткой для набора барабанов.
--
-- Слово из паттерна всегда попадает в 'noteLabel', поэтому инструмент может
-- смотреть и на частоту, и на имя.
--
-- Не нота и не число это частота ноль, а не ошибка: так пишутся барабаны
-- (@\"bd*4\"@). Мелодическая опечатка (@c22@) поэтому даёт тишину молча -
-- 'noteHz' в параметрах патча, наоборот, падает. Амплитуда всегда единичная,
-- синтаксиса под неё нет: меняют её через @fmap@.
notes :: String -> Pattern Note
notes = fmap toNote . parsePat
  where
    toNote w = (noteOf (freqOf w)) {noteLabel = w}
    freqOf w = case noteHzOf w of
      Just f -> f
      Nothing -> case reads w of
        [(v, "")] -> v
        _ -> 0

-- | Строка это паттерн нот: @"a1 ~ a1 c2"@, @"bd*4"@, @"55 73.42"@.
instance IsString (Pattern Note) where
  fromString = notes

-- | Частота по имени ноты, для параметров патчей: там паттерна нет, а имена
-- читаются лучше герцев. Незнакомое имя это опечатка автора, поэтому падает
-- сразу, а не подставляет тишину.
noteHz :: String -> Double
noteHz src = fromMaybe (error ("hsig: не нота: " <> src)) (noteHzOf src)
