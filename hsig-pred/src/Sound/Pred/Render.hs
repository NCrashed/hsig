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
  , voiceLinesIn
  , degreeLine
  , hzOf
  , chunksOf
  , runsOf
  , firstOfRun
  , sustained
  , harmonyPattern
  , melodyPattern
  , accentPattern
  ) where

import Data.List (sort)
import Sound.Pred.Orbifold
import Sound.Sig.Score (Note (..), Pattern, cat, fastcat, noteOf, stack, timecat)

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
voiceLines sc base = voiceLinesIn sc base defaultRange

-- | Полудиапазон голоса в полутонах по умолчанию.
--
-- Десять, а не семь, и это измерено. При семи голос упирается в край окна
-- на каждом пятом шаге, и октавные возвраты становятся слышной частью
-- фактуры вместо редкой поправки. При десяти доля малых шагов выше девяти
-- десятых, а суммарный разброс остаётся около двух октав.
defaultRange :: Double
defaultRange = 10

-- | То же с явным полудиапазоном каждого голоса.
--
-- Диапазон обязателен, и это не украшение. Без него выбор ближайшей октавы
-- превращается в случайное блуждание: шаг не больше полутона по кругу, но
-- ограничения нет, и за несколько сотен событий голос уходит на октавы.
-- Слышно это как медленное уползание вверх, от которого делается физически
-- неприятно.
--
-- Каждый голос помнит высоту, с которой начал, и дальше не отходит от неё
-- дальше полудиапазона. Внутри окна выбирается ближайшая к предыдущей
-- высоте октава, то есть гладкость сохраняется; окно только не даёт
-- накапливаться сносу.
voiceLinesIn :: Scale -> Double -> Double -> [Chord] -> [[Double]]
voiceLinesIn sc base halfRange = go Nothing . map (mkChord . chordDegrees)
  where
    go _ [] = []
    go Nothing (c : cs) = v : go (Just (c, v, v)) cs
      where
        v = spread (chordSemis sc c)
    go (Just (pc, pv, home)) (c : cs) = v : go (Just (c, v, home)) cs
      where
        v =
          [ pick ctr p cur
          | (ctr, (p, (_, cur))) <- zip home (zip pv (matchSemis sc pc c))
          ]

    -- Октавная копия ступени: ближайшая к прежней высоте среди тех, что не
    -- вышли за окно. Если окно не достаётся ни одной, берётся ближайшая к
    -- центру окна - лучше скачок, чем уход навсегда.
    pick ctr prev cur = case filter inWindow cands of
      [] -> closestTo ctr cands
      ok -> closestTo prev ok
      where
        x = fromIntegral cur
        cands = [x + 12 * fromIntegral k | k <- [-5 .. 5 :: Int]]
        inWindow y = abs (y - ctr) <= halfRange
        closestTo t = foldr1 (\a b -> if abs (a - t) <= abs (b - t) then a else b)

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
--
-- Годится держащему голосу. Щипковому не годится: у него «нет новой атаки»
-- означает не удержание, а тишину, и такт из двух слитых пробегов слышится
-- как две ноты и пауза, а не как фраза. Для такого голоса есть
-- 'accentPattern'.
melodyPattern :: Double -> Int -> [Double] -> Pattern Note
melodyPattern base n = sustained n (pure . noteOf . hzOf base)

-- | Отметки начала пробега: первое событие истинно, повторы ложны.
firstOfRun :: (Eq a) => [a] -> [Bool]
firstOfRun [] = []
firstOfRun (x : xs) = True : zipWith (/=) xs (x : xs)

-- | Информация в акцент, а не в длительность.
--
-- Атака на каждом событии, но повтор берётся тише. Тот же принцип, что и в
-- 'sustained' - предсказуемое событие не заслуживает полной атаки - только
-- выражен громкостью, потому что для щипкового голоса удержание недоступно.
-- Именно так это и играют: ударник и чиптюновый трекер отмечают новое
-- акцентом, а не паузой.
--
-- Громкость кладётся в 'noteAmp', и учитывать её обязан инструмент: рендер
-- за него этого не делает.
accentPattern :: Double -> Double -> Int -> [Double] -> Pattern Note
accentPattern base quiet n ss =
  cat
    [ fastcat [pure (hit a s) | (a, s) <- bar]
    | bar <- chunksOf n (zip amps ss)
    ]
  where
    amps = [if new then 1 else quiet | new <- firstOfRun ss]
    hit a s = (noteOf (hzOf base s)) {noteAmp = a}
