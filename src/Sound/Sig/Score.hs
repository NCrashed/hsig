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

    -- * Мини-нотация
  , parsePat
  , numbers

    -- * Ноты
  , Note (..)
  , Instrument
  , noteOf
  ) where

import Data.Char (isDigit, isSpace)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

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
degradeSeeded seed amount p = Pattern $ \a -> filter keep (queryArc p a)
  where
    keep e = randomAt (fromMaybe (eventPart e) (eventWhole e)) >= amount
    randomAt (Arc s e) = doubleAt seed (hashTime ((s + e) / 2))
    hashTime t = fromIntegral (numerator t) * 2654435761 + fromIntegral (denominator t)

-- Мини-нотация --------------------------------------------------------------

-- | Строка это паттерн: @"bd*4"@, @"bd ~ sn ~"@, @"[bd sn] cp"@.
instance IsString (Pattern String) where
  fromString = parsePat

-- | Разбор мини-нотации Tidal.
--
-- Поддержано ядро: последовательность делит цикл, @~@ пауза, @*n@ ускорение,
-- @\/n@ замедление, @[..]@ подгруппа в один слот, @\<..\>@ смена по циклам,
-- запятая наложение, @?@ прореживание вдвое.
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
  | TQuest
  deriving (Eq, Show)

tokenize :: String -> [Tok]
tokenize [] = []
tokenize (c : cs)
  | isSpace c = tokenize cs
  | Just t <- lookup c punctuation = t : tokenize cs
  | otherwise = let (w, rest) = span plain (c : cs) in TWord w : tokenize rest
  where
    plain ch = not (isSpace ch) && ch `notElem` map fst punctuation

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
  , ('?', TQuest)
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
    -- Номер потока берём от позиции в строке (сколько токенов осталось):
    -- разные ? одной строки обязаны решать независимо.
    mods (p, TQuest : rest) = mods (degradeSeeded (length rest) 0.5 p, rest)
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
  , noteParams :: !(Map String Double)
  }
  deriving (Eq, Show)

-- | Сигнал ноты начинается с нуля и конечен.
type Instrument = Note -> Sig

-- | Нота по частоте: амплитуда единичная, параметров нет.
noteOf :: Double -> Note
noteOf f =
  Note
    { noteOnset = 0
    , noteDur = 0
    , noteFreq = f
    , noteAmp = 1
    , noteParams = Map.empty
    }
