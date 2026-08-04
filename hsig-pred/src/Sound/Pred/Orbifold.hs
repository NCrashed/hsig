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
-- Отсюда же берётся благозвучие: не фильтром по правилам, а тем, что поиск
-- идёт по терцовым фигурам, а не по произвольным тройкам ступеней. Одного
-- лада для этого мало: в нём собирается и кластер (см. 'defaultEmbed').
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
  , embedVoices
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
  , embedShapes :: [[Int]]
  -- ^ допустимые фигуры голосов как смещения ступеней от основания
  , embedSpan :: Double
  -- ^ во сколько полутонов растягивается самое большое расстояние модели
  , embedRounds :: Int
  -- ^ потолок проходов локального поиска
  }

-- | Голосов в аккорде: длина фигуры.
embedVoices :: EmbedOpts -> Int
embedVoices opts = case embedShapes opts of
  (s : _) -> length s
  [] -> error "hsig-pred: пустой набор фигур"

-- | Мажор, терцовое трезвучие, самое далёкое состояние в шести полутонах.
--
-- Фигура терцовая не по традиции, а по необходимости. Поиск по произвольным
-- тройкам ступеней остаётся в ладу, но благозвучия это не даёт: ступени
-- 1, 2, 4 дают малую секунду, а 2, 3, 6 - секунду с тритоном. На органе с
-- шестью частичными такое бьётся обертонами и звучит отвратительно.
--
-- Утверждение «благозвучие берётся из того, что поиск идёт по ступеням»
-- было неверным и опровергнуто на слух. Верное: благозвучие берётся из
-- того, что поиск идёт по **фигурам**. Терцовая стопка в семиступенном
-- ладу консонантна по построению, и отдельный запрет на секунды не нужен -
-- собрать её просто не из чего.
defaultEmbed :: EmbedOpts
defaultEmbed =
  EmbedOpts
    { embedScale = mkScale "major"
    , embedShapes = [[0, 2, 4]]
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
-- Локальный поиск по ступеням из нескольких детерминированных стартов, без
-- случайности. Воспроизводимость тут важнее оптимума, а качество укладки
-- всё равно меряется отдельно ('lipschitz', 'distortion'), а не
-- предполагается.
embed :: EmbedOpts -> [[Double]] -> [Chord]
embed opts d = map chordAt (lowest [go (embedRounds opts) s | s <- starts])
  where
    n = length d
    shapes = embedShapes opts

    -- Позиция состояния это основание и номер фигуры. Аккорд собирается из
    -- них, поэтому нот вне фигуры не бывает вовсе: искать негде.
    chordAt (r, si) = mkChord [r + o | o <- shapes !! si]

    -- Несколько стартов, а не один. Координатный спуск застревает: два
    -- состояния с небольшим, но ненулевым расстоянием садятся на один
    -- аккорд, и сдвинуть любое из них поодиночке дороже, чем оставить.
    -- Признак в измерении однозначный - искажение уходит в бесконечность.
    -- Случайности не добавлено, воспроизводимость дороже.
    starts =
      [ [(off + i * step, 0) | i <- [0 .. n - 1]]
      | step <- [1, 2, 3]
      , off <- [0, 1]
      ]

    stressAt cs = stress opts d (map chordAt cs)
    lowest = foldr1 (\a b -> if stressAt a <= stressAt b then a else b)

    go 0 cs = cs
    go r cs
      | stressAt cs' < stressAt cs - 1e-12 = go (r - 1) cs'
      | otherwise = cs
      where
        cs' = sweep cs

    -- Один проход: каждое состояние по очереди уезжает в лучшую соседнюю
    -- позицию при замороженных остальных.
    sweep cs = foldl improve cs [0 .. n - 1]

    improve cs i = pick cs [replaceAt i p cs | p <- neighbours (cs !! i), free cs i p]
      where
        pick acc [] = acc
        pick acc (x : xs)
          | stressAt x < stressAt acc = pick x xs
          | otherwise = pick acc xs

    -- Два состояния не могут получить один аккорд. Это не вкус, а условие
    -- осмысленности: гармония объявлена носителем причинного состояния, и
    -- совпадение означает, что состояние ею не кодируется. Стресс такое
    -- допускает охотно, поэтому запрет стоит в поиске, а не в оценке.
    --
    -- Аккорды сравниваются по звучанию, то есть по классам высот: основания,
    -- отличающиеся на размер лада, дают один и тот же аккорд.
    free cs i p = all (\j -> pcs (chordAt (cs !! j)) /= pcs (chordAt p)) others
      where
        others = [j | j <- [0 .. n - 1], j /= i]
    pcs c = sort (map (`mod` 12) (chordSemis (embedScale opts) c))

    -- Соседи по сдвигу основания и по смене фигуры.
    neighbours (r, si) =
      [(r + delta, si) | delta <- [-2, -1, 1, 2]]
        <> [(r, sj) | sj <- [0 .. length shapes - 1], sj /= si]

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
