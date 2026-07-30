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
-- Поддержано ядро: последовательность делит цикл, @~@ пауза, @*n@ ускорение,
-- @\/n@ замедление, @[..]@ подгруппа в один слот, @\<..\>@ смена по циклам,
-- запятая наложение, @?@ прореживание вдвое, @?0.3@ прореживание с явной
-- долей (точка обязательна, как в Tidal). У каждого @?@ свой поток
-- случайности, номер по порядку вхождения.
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
      | otherwise = let (w, rest) = span plain (c : cs) in TWord w : go rest
    plain ch = ch /= '?' && not (isSpace ch) && ch `notElem` map fst punctuation

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
  ]

-- | Слои через запятую.
parseStack :: [Tok] -> (Pattern String, [Tok])
parseStack ts = case parseSeq ts of
  (p, TComma : more) -> let (q, rest) = parseStack more in (stack [p, q], rest)
  (p, rest) -> (p, rest)

-- | Последовательность делит цикл между своими элементами.
parseSeq :: [Tok] -> (Pattern String, [Tok])
parseSeq = go []
  where
    go acc ts
      | stop ts = (done (reverse acc), ts)
      | otherwise = let (t, rest) = parseTerm ts in go (t : acc) rest
    stop ts = case ts of
      [] -> True
      TClose : _ -> True
      TUnangle : _ -> True
      TComma : _ -> True
      _ -> False
    done ps = case ps of
      [] -> silence
      [p] -> p
      _ -> fastcat ps

-- | Элемент с модификаторами.
parseTerm :: [Tok] -> (Pattern String, [Tok])
parseTerm ts = mods (parseAtom ts)
  where
    mods (p, TStar : TWord w : rest) = mods (fast (rate w) p, rest)
    mods (p, TSlash : TWord w : rest) = mods (slow (rate w) p, rest)
    -- Номер потока приходит готовым из numberQuests: он равен числу
    -- вопросов левее, поэтому разные ? одной строки решают независимо.
    mods (p, TQuest k amt : rest) = mods (degradeSeeded k amt p, rest)
    mods (_, TStar : rest) = bad ("после * нужно число: " <> show (take 1 rest))
    mods (_, TSlash : rest) = bad ("после / нужно число: " <> show (take 1 rest))
    mods done = done
    rate w = fromMaybe (bad ("не число: " <> w)) (decimal w)

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
  _ -> bad ("неожидан токен: " <> show (take 1 ts))
  where
    angleItems acc rest = case rest of
      TUnangle : _ -> (reverse acc, rest)
      [] -> (reverse acc, rest)
      _ -> let (t, more) = parseTerm rest in angleItems (t : acc) more

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
