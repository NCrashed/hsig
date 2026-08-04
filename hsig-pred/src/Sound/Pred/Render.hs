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
  , hzOf
  , chunksOf
  , harmonyPattern
  , melodyPattern
  ) where

import Data.List (sort)
import Sound.Pred.Orbifold
import Sound.Sig.Score (Note, Pattern, cat, fastcat, noteOf, stack)

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

-- | Аккорды в партитуру: такт это цикл, внутри такта события подряд,
-- голоса одновременно.
harmonyPattern :: Double -> Int -> [[Double]] -> Pattern Note
harmonyPattern base n evs =
  cat
    [ fastcat [stack [pure (noteOf (hzOf base s)) | s <- voices] | voices <- bar]
    | bar <- chunksOf n evs
    ]

-- | Мелодия: по ноте на событие, той же нарезкой по тактам.
melodyPattern :: Double -> Int -> [Double] -> Pattern Note
melodyPattern base n ss =
  cat
    [ fastcat [pure (noteOf (hzOf base s)) | s <- bar]
    | bar <- chunksOf n ss
    ]
