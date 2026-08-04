-- | Пространство тембров: укладка состояний туда, где носителем служит
-- фактура, а не высота.
--
-- Три координаты, и все три выбраны потому, что измеримы у чужой записи
-- тем же `tools/analyze.sh`, которым мерялся референс:
--
-- * яркость - спектральный центр тяжести, в октавах от опорной частоты;
-- * шумность - спектральная плоскостность, ноль тон, единица шум;
-- * резкость - время атаки, в октавах от миллисекунды.
--
-- Метрика взвешенная евклидова. Веса не подобраны на слух, а взяты из
-- различительной способности: октава яркости слышна примерно как переход
-- от тона к шуму и примерно как удесятерение времени атаки, поэтому все три
-- приведены к сопоставимым шкалам и складываются наравне.
--
-- Решётка целочисленная. Причина та же, что у терцовых фигур в аккордовом
-- пространстве: непрерывный поиск охотно садится в щели, которые слухом не
-- различаются, и укладка перестаёт кодировать состояние.
module Sound.Pred.Timbre
  ( Timbre (..)
  , timbreSpace
  , timbreDist
  , timbreGrid
  , brightHz
  , attackSec
  ) where

import Sound.Pred.Embed

-- | Точка решётки тембров. Единица каждой координаты это один слышимый шаг.
data Timbre = Timbre
  { timbreBright :: Int
  -- ^ яркость: 0 это 80 Гц, шаг пол-октавы
  , timbreNoise :: Int
  -- ^ шумность: 0 это чистый тон, 6 это шум
  , timbreBite :: Int
  -- ^ резкость: 0 это медленная атака 200 мс, 6 это щелчок 1 мс
  }
  deriving (Eq, Ord, Show)

-- | Центр тяжести точки в герцах.
brightHz :: Timbre -> Double
brightHz t = 80 * 2 ** (fromIntegral (timbreBright t) / 2)

-- | Время атаки точки в секундах.
attackSec :: Timbre -> Double
attackSec t = 0.2 * 2 ** (negate (fromIntegral (timbreBite t)) * 1.27)

-- | Шумность от нуля до единицы.
noiseAmount :: Timbre -> Double
noiseAmount t = fromIntegral (timbreNoise t) / 6

-- | Перцептивное расстояние.
--
-- Яркость делится на два, потому что её шаг это пол-октавы, а октава взята
-- за единицу слышимого различия. Шумность и резкость уже нормированы на
-- свои полные диапазоны.
timbreDist :: Timbre -> Timbre -> Double
timbreDist a b = sqrt (bright ** 2 + noise ** 2 + bite ** 2)
  where
    bright = fromIntegral (timbreBright a - timbreBright b) / 2
    noise = 2 * (noiseAmount a - noiseAmount b)
    bite = fromIntegral (timbreBite a - timbreBite b) / 3

-- | Допустимая решётка: яркость от 80 Гц до пяти килогерц, шумность и
-- резкость по семь ступеней.
timbreGrid :: [Timbre]
timbreGrid =
  [ Timbre br ns bt
  | br <- [0 .. 12]
  , ns <- [0 .. 6]
  , bt <- [0 .. 6]
  ]

-- | Пространство тембров для 'embedIn'.
--
-- Соседство по одному шагу любой координаты: поиск ползёт по решётке, а не
-- прыгает, поэтому промежуточные положения тоже проверяются.
--
-- Стартовые раскладки разводят состояния по разным осям: если все стартуют
-- вдоль яркости, поиск и остаётся на этой оси, а шумность с резкостью
-- пропадают зря.
timbreSpace :: Space Timbre
timbreSpace =
  Space
    { spaceNeighbours = \t ->
        [ t'
        | t' <-
            [ t {timbreBright = timbreBright t + d} | d <- [-1, 1]
            ]
              <> [t {timbreNoise = timbreNoise t + d} | d <- [-1, 1]]
              <> [t {timbreBite = timbreBite t + d} | d <- [-1, 1]]
        , t' `elem` timbreGrid
        ]
    , spaceDist = timbreDist
    , spaceStarts = \n ->
        [ [along i n | i <- [0 .. n - 1]]
        | along <-
            [ \i k -> Timbre (12 * i `div` max 1 (k - 1)) 3 3
            , \i k -> Timbre 6 (6 * i `div` max 1 (k - 1)) 3
            , \i k -> Timbre 6 3 (6 * i `div` max 1 (k - 1))
            , \i k -> Timbre (12 * i `div` max 1 (k - 1)) (6 * i `div` max 1 (k - 1)) 3
            , \i k -> Timbre (12 * i `div` max 1 (k - 1)) 3 (6 * i `div` max 1 (k - 1))
            ]
        ]
    , -- Одинаково звучащими считаются совпадающие точки решётки: шаг решётки
      -- и есть порог различимости, ради этого она целочисленная.
      spaceSame = (==)
    }
