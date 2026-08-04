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
import Sound.Pred.Embed
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

-- | Невязка укладки: сумма квадратов отклонений голосоведения от цели.
stress :: EmbedOpts -> [[Double]] -> [Chord] -> Double
stress opts d cs = stressIn (flatSpace opts) (embedSpan opts) d cs

-- | Пространство, где позиция это сам аккорд: нужно только чтобы померить
-- стресс готовой раскладки, поиск по нему не ходит.
flatSpace :: EmbedOpts -> Space Chord
flatSpace opts =
  Space
    { spaceNeighbours = const []
    , spaceDist = vlDist (embedScale opts)
    , spaceStarts = const []
    , spaceSame = \a b -> pcsOf opts a == pcsOf opts b
    }

-- | Уложить состояния в аккорды так, чтобы близкие предсказания стали
-- близким голосоведением.
--
-- Локальный поиск по ступеням из нескольких детерминированных стартов, без
-- случайности. Воспроизводимость тут важнее оптимума, а качество укладки
-- всё равно меряется отдельно ('lipschitz', 'distortion'), а не
-- предполагается.
embed :: EmbedOpts -> [[Double]] -> [Chord]
embed opts d = map chordAt (embedIn (chordSpace opts) (embedSpan opts) (embedRounds opts) d)
  where
    chordAt = chordOfPos opts

-- | Пространство аккордов как частный случай общей укладки.
--
-- Позиция это основание и номер фигуры. Аккорд собирается из них, поэтому
-- нот вне фигуры не бывает вовсе - искать негде, и запрет на секунды не
-- нужен отдельным правилом.
chordSpace :: EmbedOpts -> Space (Int, Int)
chordSpace opts =
  Space
    { spaceNeighbours = \(r, si) ->
        [(r + delta, si) | delta <- [-2, -1, 1, 2]]
          <> [(r, sj) | sj <- [0 .. length (embedShapes opts) - 1], sj /= si]
    , spaceDist = \a b -> vlDist (embedScale opts) (chordOfPos opts a) (chordOfPos opts b)
    , spaceStarts = \n ->
        [ [(off + i * step, 0) | i <- [0 .. n - 1]]
        | step <- [1, 2, 3]
        , off <- [0, 1]
        ]
    , spaceSame = \a b -> pcsOf opts (chordOfPos opts a) == pcsOf opts (chordOfPos opts b)
    }

chordOfPos :: EmbedOpts -> (Int, Int) -> Chord
chordOfPos opts (r, si) = mkChord [r + o | o <- embedShapes opts !! si]

-- | Классы высот аккорда: сравнение по звучанию, а не по записи.
pcsOf :: EmbedOpts -> Chord -> [Int]
pcsOf opts c = sort (map (`mod` 12) (chordSemis (embedScale opts) c))

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
lipschitz sc cs d = lipschitzIn (metricSpace sc) cs d

-- | Билипшицево искажение: отношение наибольшего растяжения к наименьшему.
-- Единица означает подобие, чем больше, тем хуже слышна структура модели.
distortion :: Scale -> [Chord] -> [[Double]] -> Double
distortion sc cs d = distortionIn (metricSpace sc) cs d

-- | Пространство только с метрикой: для замеров качества готовой укладки.
metricSpace :: Scale -> Space Chord
metricSpace sc =
  Space
    { spaceNeighbours = const []
    , spaceDist = vlDist sc
    , spaceStarts = const []
    , spaceSame = \a b -> vlDist sc a b == 0
    }
