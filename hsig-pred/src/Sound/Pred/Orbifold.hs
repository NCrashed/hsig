-- | Пространство аккордов и липшицево вложение состояний.
--
-- Главное решение: **симметрия диатоническая, метрика хроматическая**.
--
-- Голоса живут в ступенях лада, поэтому перенос это прибавление целого и
-- аккорд из лада не выходит никогда. Расстояние же меряется в полутонах,
-- потому что ухо слышит полутоны, а не ступени. Из-за этого перенос по
-- ступеням не сохраняет интервалы: тоническое трезвучие сдвигом на ступень
-- превращается из мажорного в минорное. Это не дефект отображения, это то
-- самое различие между реальным и тональным ответом, на котором держится
-- фуга.
--
-- Отсюда же берётся благозвучие: не фильтром по правилам, а тем, что
-- поиск идёт по ступеням. Если понадобился отдельный запрет на созвучие,
-- значит неправ выбор пространства (docs/PRED.md, разд. 9).
module Sound.Pred.Orbifold
  ( -- * Лад
    Scale (..)
  , mkScale

    -- * Аккорды
  , Chord (..)
  , mkChord
  , chordSemis
  , transposeDeg

    -- * Метрика голосоведения
  , vlDist
  , matchSemis

    -- * Вложение
  , EmbedOpts (..)
  , defaultEmbed
  , embed
  , stress
  , lipschitz
  , distortion
  , distinctVoices
  ) where

import Data.List (nub, permutations, sort)
import Data.Maybe (fromMaybe)
import Sound.Sig.Harmony (degreeSemitones, scaleSemitones)

-- Лад ------------------------------------------------------------------------

-- | Лад: имя и подъёмы ступеней в полутонах.
data Scale = Scale
  { scaleName :: String
  , scaleSteps :: [Int]
  }
  deriving (Eq, Show)

-- | Лад по имени из таблицы hsig. Неизвестное имя это ошибка сборки пьесы,
-- а не повод молча взять хроматику.
mkScale :: String -> Scale
mkScale name = Scale name (fromMaybe bad (scaleSemitones name))
  where
    bad = error ("hsig-pred: нет такого лада: " <> name)

-- Аккорды --------------------------------------------------------------------

-- | Мультимножество ступеней лада. Ступени не приводятся по модулю:
-- регистр несёт смысл, а сравнение по звучанию делает 'vlDist', а не 'Eq'.
newtype Chord = Chord {chordDegrees :: [Int]}
  deriving (Eq, Ord, Show)

-- | Аккорд с приведением к каноническому порядку голосов.
mkChord :: [Int] -> Chord
mkChord = Chord . sort

-- | Ступени в полутоны от основы лада.
chordSemis :: Scale -> Chord -> [Int]
chordSemis sc (Chord ds) = map (degreeSemitones (scaleSteps sc)) ds

-- | Диатонический перенос: сдвиг по ступеням.
--
-- Точная симметрия пространства состояний и одновременно искажение
-- полутоновых интервалов. Хроматический сдвиг вёл бы себя ровно наоборот:
-- сохранял интервалы и выводил из лада.
transposeDeg :: Int -> Chord -> Chord
transposeDeg k (Chord ds) = Chord (map (+ k) ds)

-- Метрика --------------------------------------------------------------------

-- | Расстояние голосоведения в полутонах: минимальное по сопоставлению
-- голосов суммарное движение, каждый голос по кратчайшей дуге октавы.
--
-- Это назначенческое расстояние на мультимножествах с круговой метрикой,
-- то есть настоящая метрика, а не эвристика. Перебор всех сопоставлений
-- честный: голосов три-четыре, точность важнее скорости (DESIGN.md,
-- разд. 1).
vlDist :: Scale -> Chord -> Chord -> Double
vlDist sc a b = fst (assign sc a b)

-- | Пары полутонов в оптимальном сопоставлении: слева голос первого
-- аккорда, справа тот, в который он ведёт.
--
-- Нужно рендеру: 'vlDist' забывает, кто куда пошёл, а слышно именно это.
-- Перекрещивание голосов не симметрия, а косичка, и ухо её отслеживает.
matchSemis :: Scale -> Chord -> Chord -> [(Int, Int)]
matchSemis sc a b = zip (chordSemis sc a) (snd (assign sc a b))

-- | Оптимальное назначение голосов: стоимость и переставленный второй
-- аккорд. Перебор честный, голосов три-четыре.
assign :: Scale -> Chord -> Chord -> (Double, [Int])
assign sc a b
  | length xs /= length ys = error "hsig-pred: аккорды разной голосности"
  | otherwise = minimum [(sum (zipWith circ xs p), p) | p <- permutations ys]
  where
    xs = chordSemis sc a
    ys = chordSemis sc b
    circ u v = fromIntegral (min d (12 - d))
      where
        d = (v - u) `mod` 12

-- Вложение --------------------------------------------------------------------

-- | Настройки укладки состояний в аккорды.
data EmbedOpts = EmbedOpts
  { embedScale :: Scale
  , embedVoices :: Int
  -- ^ голосов в аккорде
  , embedSpan :: Double
  -- ^ во сколько полутонов растягивается самое большое расстояние модели
  , embedRounds :: Int
  -- ^ потолок проходов локального поиска
  }

-- | Мажор, три голоса, самое далёкое состояние в шести полутонах.
defaultEmbed :: EmbedOpts
defaultEmbed =
  EmbedOpts
    { embedScale = mkScale "major"
    , embedVoices = 3
    , embedSpan = 6
    , embedRounds = 60
    }

-- | Целевые расстояния: матрица модели, растянутая до 'embedSpan'.
targets :: EmbedOpts -> [[Double]] -> [[Double]]
targets opts d = [[k * x | x <- row] | row <- d]
  where
    top = maximum (0 : concat d)
    k = if top > 0 then embedSpan opts / top else 0

-- | Невязка укладки: сумма квадратов отклонений голосоведения от цели.
stress :: EmbedOpts -> [[Double]] -> [Chord] -> Double
stress opts d cs =
  sum
    [ (vlDist (embedScale opts) (cs !! i) (cs !! j) - tg !! i !! j) ** 2
    | i <- [0 .. n - 1]
    , j <- [i + 1 .. n - 1]
    ]
  where
    n = length cs
    tg = targets opts d

-- | Уложить состояния в аккорды так, чтобы близкие предсказания стали
-- близким голосоведением.
--
-- Локальный поиск по ступеням: детерминированный, без случайных стартов.
-- Воспроизводимость тут важнее оптимума, а качество укладки всё равно
-- меряется отдельно ('lipschitz', 'distortion'), а не предполагается.
embed :: EmbedOpts -> [[Double]] -> [Chord]
embed opts d = go (embedRounds opts) start
  where
    n = length d
    v = embedVoices opts
    -- Стартовая раскладка по терциям со сдвигом на номер состояния:
    -- заведомо различные аккорды в пределах лада.
    start = [mkChord [i + 2 * j | j <- [0 .. v - 1]] | i <- [0 .. n - 1]]

    go 0 cs = cs
    go r cs
      | stress opts d cs' < stress opts d cs - 1e-12 = go (r - 1) cs'
      | otherwise = cs
      where
        cs' = sweep cs

    -- Один проход: каждое состояние по очереди уезжает в лучшую соседнюю
    -- позицию при замороженных остальных.
    sweep cs = foldl improve cs [0 .. n - 1]

    improve cs i = best
      where
        cands = [replaceAt i c cs | c <- neighbours (cs !! i)]
        best = pick cs cands
        pick acc [] = acc
        pick acc (x : xs)
          | stress opts d x < stress opts d acc = pick x xs
          | otherwise = pick acc xs

    -- Соседи по одному сдвигу голоса. Аккорды с двумя голосами на одной
    -- высоте отбрасываются: поиск охотно схлопывает голоса, потому что это
    -- дёшево по стрессу, а звучит как потеря голоса.
    neighbours (Chord ds) =
      [ c
      | j <- [0 .. length ds - 1]
      , delta <- [-2, -1, 1, 2]
      , let c = mkChord (replaceAt j (ds !! j + delta) ds)
      , distinctVoices (embedScale opts) c
      ]

    replaceAt i x xs = take i xs <> [x] <> drop (i + 1) xs

-- | Все голоса аккорда на разных высотах по модулю октавы.
distinctVoices :: Scale -> Chord -> Bool
distinctVoices sc c = length (nub pcs) == length pcs
  where
    pcs = map (`mod` 12) (chordSemis sc c)

-- | Фактическая липшицева константа укладки: наибольшее отношение
-- голосоведения к расстоянию модели.
--
-- Меряется, а не декларируется. Пары с нулевым расстоянием модели
-- пропускаются: они ничего не ограничивают.
lipschitz :: Scale -> [Chord] -> [[Double]] -> Double
lipschitz sc cs d = maximum (0 : ratios sc cs d)

-- | Билипшицево искажение: отношение наибольшего растяжения к наименьшему.
-- Единица означает подобие, чем больше, тем хуже слышна структура модели.
distortion :: Scale -> [Chord] -> [[Double]] -> Double
distortion sc cs d
  | null rs = 1
  | lo <= 0 = 1 / 0
  | otherwise = maximum rs / lo
  where
    rs = ratios sc cs d
    lo = minimum rs

ratios :: Scale -> [Chord] -> [[Double]] -> [Double]
ratios sc cs d =
  [ vlDist sc (cs !! i) (cs !! j) / (d !! i !! j)
  | i <- [0 .. n - 1]
  , j <- [i + 1 .. n - 1]
  , d !! i !! j > 0
  ]
  where
    n = length cs
