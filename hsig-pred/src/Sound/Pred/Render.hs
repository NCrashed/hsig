-- | Из аккордов в партитуру hsig.
--
-- Здесь и только здесь появляется ось времени, поэтому здесь же решается
-- регистр. 'Sound.Pred.Orbifold' живёт по модулю октавы: это правильно для
-- метрики, но петь так нельзя. Голоса разводятся по регистрам и дальше
-- каждый держится своей высоты, выбирая ближайшую октаву к предыдущей.
--
-- Тождество голоса сохраняется намеренно. Метрика голосоведения берёт
-- минимум по сопоставлениям и тем самым забывает, кто через кого прошёл, а
-- ухо это как раз слышит: перекрещивание голосов не перестановка, а
-- косичка.
module Sound.Pred.Render
  ( voiceLines
  , degreeLine
  , hzOf
  , chunksOf
  , runsOf
  , sustained
  , harmonyPattern
  , melodyPattern
  ) where

import Data.List (sort)
import Sound.Pred.Orbifold
import Sound.Sig.Score (Note, Pattern, cat, noteOf, stack, timecat)

-- | Полутоны в герцы; @base@ это частота нулевого полутона.
hzOf :: Double -> Double -> Double
hzOf base s = base * 2 ** (s / 12)

-- | Разбить список на куски заданной длины.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | n <= 0 = error "hsig-pred: chunksOf с непозитивной длиной"
  | null xs = []
  | otherwise = take n xs : chunksOf n (drop n xs)

-- | Последовательность аккордов в абсолютные полутоны по голосам.
--
-- Первый аккорд раскладывается снизу вверх от @base@, дальше каждый голос
-- идёт в ближайшую октаву своей новой ступени. Скачков больше чем на
-- полтона по кругу не бывает по построению.
voiceLines :: Scale -> Double -> [Chord] -> [[Double]]
voiceLines sc base = go Nothing . map (mkChord . chordDegrees)
  where
    go _ [] = []
    go Nothing (c : cs) = v : go (Just (c, v)) cs
      where
        v = spread (chordSemis sc c)
    go (Just (pc, pv)) (c : cs) = v : go (Just (c, v)) cs
      where
        v = [nearest p cur | (p, (_, cur)) <- zip pv (matchSemis sc pc c)]

    -- Ближайшая октавная копия ступени к прежней высоте голоса.
    nearest prev cur = x + 12 * fromIntegral (round ((prev - x) / 12) :: Int)
      where
        x = fromIntegral cur

    -- Снизу вверх от base, каждый следующий голос выше предыдущего.
    spread ss = reverse (foldl place [] (sort ss))
      where
        place acc x = liftTo bottom (fromIntegral x) : acc
          where
            bottom = case acc of
              [] -> base
              (p : _) -> p + 0.5
        liftTo bottom v = if v >= bottom then v else liftTo bottom (v + 12)

-- | Одноголосная линия из ступеней: тот же выбор ближайшей октавы, что и у
-- голосов аккорда, поэтому мелодия не прыгает через регистр на границе
-- октавы лада.
degreeLine :: Scale -> Double -> [Int] -> [Double]
degreeLine sc base = map firstVoice . voiceLines sc base . map (\d -> mkChord [d])
  where
    firstVoice (x : _) = x
    firstVoice [] = error "hsig-pred: пустой голос в линии"

-- | Соседние одинаковые значения в пары «сколько подряд, что именно».
runsOf :: (Eq a) => [a] -> [(Int, a)]
runsOf [] = []
runsOf (x : xs) = (1 + length same, x) : runsOf rest
  where
    (same, rest) = span (== x) xs

-- | Партитура, где длительность ноты берётся из информации, а не из сетки.
--
-- Событие, ничего не изменившее, не получает новой атаки: оно продлевает
-- предыдущую ноту. Это не украшение, а прямое следствие того, чем здесь
-- считается смысл - предсказуемое событие не несёт информации, и отбивать
-- его отдельно значит врать слуху о содержании.
--
-- Слияние идёт только внутри такта. Метрическая сетка задаётся снаружи и
-- программой не выводится (docs/PRED.md, разд. 2), поэтому границу такта
-- нота не переходит.
sustained :: (Eq a) => Int -> (a -> Pattern Note) -> [a] -> Pattern Note
sustained n toPat evs =
  cat
    [ timecat [(fromIntegral k, toPat v) | (k, v) <- runsOf bar]
    | bar <- chunksOf n evs
    ]

-- | Аккорды в партитуру: такт это цикл, голоса одновременно, повторы слиты.
harmonyPattern :: Double -> Int -> [[Double]] -> Pattern Note
harmonyPattern base n = sustained n chord
  where
    chord voices = stack [pure (noteOf (hzOf base s)) | s <- voices]

-- | Мелодия: по ноте на событие той же нарезкой, повторы слиты.
melodyPattern :: Double -> Int -> [Double] -> Pattern Note
melodyPattern base n = sustained n (pure . noteOf . hzOf base)
