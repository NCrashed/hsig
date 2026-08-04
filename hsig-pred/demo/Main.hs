-- | Первая пьеса: предиктивная модель, уложенная в аккорды и сыгранная.
--
-- Что здесь проверяется на слух, а не тестом: слышна ли статистическая
-- сложность. Гармония несёт причинное состояние машины, мелодия несёт
-- излучённый символ. Состояния, предсказывающие похоже, уложены рядом по
-- голосоведению, поэтому близость в модели обязана звучать как близость
-- гармонии.
module Main (main) where

import Data.Function ((&))
import Sound.Pred.Compose
import Sound.Pred.Diagram
import Sound.Pred.Dist qualified as D
import Sound.Pred.Machine
import Sound.Pred.Metric (distMatrix)
import Sound.Pred.Model (unfoldPred)
import Sound.Pred.Orbifold
import Sound.Pred.Render
import Sound.Sig
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Printf (printf)

-- Ядро ------------------------------------------------------------------------

-- | Смещённое блуждание по кольцу из шести позиций.
--
-- Символ говорит, куда шагнули, поэтому машина унифилярна: состояние
-- восстанавливается по истории однозначно. Позиции 0 и 3 устойчивы и любят
-- задерживаться, остальные предпочитают движение в свою сторону. Без этого
-- различия все шесть состояний были бы бисимилярны, метрика схлопнулась бы
-- в ноль, а укладка потеряла бы смысл.
ring :: Machine Int Int
ring =
  Machine
    { machineStart = 0
    , machineStates = [0 .. 5]
    , machineOut = D.dist . weights
    , machineStep = \s x -> (s + delta x) `mod` 6
    }
  where
    delta x = case x of
      0 -> 0
      1 -> 1
      2 -> 5
      _ -> 2
    -- Распределения намеренно острые. При почти равномерных выходах
    -- h_mu подходит к двум битам, учить слушателя становится нечему, и
    -- жадный поиск начинает оптимизировать шум.
    weights s
      | s == 0 || s == 3 = [(0, 0.88), (1, 0.07), (2, 0.03), (3, 0.02)]
      | even s = [(0, 0.06), (1, 0.85), (2, 0.06), (3, 0.03)]
      | otherwise = [(0, 0.06), (1, 0.06), (2, 0.85), (3, 0.03)]

alphabet :: [Int]
alphabet = [0 .. 3]

-- | Аккорд состояния: укладка по бисимуляционной метрике.
--
-- Матрица считается один раз, дальше это просто таблица.
stateChords :: [Chord]
stateChords = embed opts (distMatrix (map asPred (machineStates ring)))
  where
    asPred s = unfoldPred (machineOut ring) (machineStep ring) s
    -- Разброс укладки не больше потолка скачка в barOpts: иначе
    -- ограничение на голосоведение нарушалось бы самой таблицей аккордов.
    opts = defaultEmbed {embedScale = tonality, embedVoices = 3, embedSpan = 4}

tonality :: Scale
tonality = mkScale "dorian"

chordOf :: Int -> Chord
chordOf s = stateChords !! s

-- Пьеса ------------------------------------------------------------------------

barOpts :: BarOpts
barOpts = defaultBarOpts {barLen = 8, barCands = 32, barVlMax = 5, barOrder = 3}

-- | Сколько тактов сочинять. Двенадцать это калибровочный прогон, на нём
-- считаются сравнения режимов отбора; длиннее задаётся аргументом.
barCount :: Int -> Int
barCount n = max 1 n

bars :: Int -> [Bar Int Int]
bars n = compose barOpts ring chordOf tonality alphabet (barCount n)

-- | Четыре режима отбора для сравнения кривых ошибки модели.
--
-- Разделяют гипотезы: виноват сам жадный критерий, виновато окно сюрприза,
-- или не виноват никто и слушатель просто не учится.
--
-- * 'bars' - умолчание: выигрыш плюс порог типичности по истинной машине;
-- * 'noTypical' - только выигрыш, без всяких ограничений;
-- * 'withWindow' - выигрыш плюс окно сюрприза, режим-виновник;
-- * 'baseline' - без выбора вообще, честная выборка из машины.
withWindow :: [Bar Int Int]
withWindow = compose barOpts {barWindow = Just (0.25, 0.8)} ring chordOf tonality alphabet 12

noTypical :: [Bar Int Int]
noTypical = compose barOpts {barTypical = Nothing} ring chordOf tonality alphabet 12

baseline :: [Bar Int Int]
baseline = compose barOpts {barCands = 1} ring chordOf tonality alphabet 12

-- | След пьесы: причинное состояние перед каждым событием и сам символ.
-- Это всё содержание трека; ниже из него выводится любая слышимая деталь.
traceOf :: [Bar Int Int] -> ([Int], [Int])
traceOf bs = (concatMap barStates bs, concatMap barSyms bs)

-- | Гармония по состояниям и мелодия по символам.
--
-- Два независимых канала: гармония несёт причинное состояние, мелодия
-- несёт излучённый символ. Смешивать их нельзя, иначе слушателю не из чего
-- разделить «где мы» и «что произошло».
linesOf :: [Bar Int Int] -> ([[Double]], [Double])
linesOf bs = (voices, melody)
  where
    (states, syms) = traceOf bs
    voices = voiceLines tonality 36 (map chordOf states)
    -- Символ выбирает ступень над основанием аккорда, а не номер голоса.
    -- При трёх голосах и четырёх символах отображение в номер склеивало бы
    -- нулевой символ с третьим, то есть теряло бы информацию ровно там,
    -- где её надо передать.
    melody = degreeLine tonality 55 [root st + 2 * s | (st, s) <- zip states syms]
    root st = minimum (chordDegrees (chordOf st))

-- Голоса ------------------------------------------------------------------------

-- | Подложка: расстроенные синусы плюс октава.
--
-- Осцилляторы в hsig аддитивные и суммируют гармоники до Найквиста
-- (DESIGN.md, разд. 2). Гармония это три голоса на каждое из событий, то
-- есть львиная доля нот трека, и богатый осциллятор тут стоит сотен
-- частичных на ноту. Характер оставлен мелодии, которой нот втрое меньше.
pad :: Instrument
pad n =
  ( sum [sine (constant (noteFreq n * d)) | d <- [0.997, 1.003]]
      + 0.35 * sine (constant (noteFreq n * 2))
  )
    * 0.24
    & ladder (700 + 900 * expdecay 0.6) 0.35
    & (* adsr 0.05 0.2 0.6 0.35 (noteDur n))

-- | Мелодия: щипок с быстрой атакой.
pluck :: Instrument
pluck n =
  (tri (constant (noteFreq n)) * 0.6 + sine (constant (noteFreq n)) * 0.4)
    & ladder (900 + 2600 * expdecay 0.09) 0.72
    & (* adsr 0.004 0.11 0.15 0.09 (noteDur n))
    & (* 0.34)

track :: [Bar Int Int] -> [Stem]
track bs =
  [ stem "pred-harm" (takeSec total (play pad (slow barSec harm)))
  , stem "pred-mel" (takeSec total (play pluck (slow barSec mel)))
  ]
  where
    (voices, melody) = linesOf bs
    harm = harmonyPattern 27.5 (barLen barOpts) voices
    mel = melodyPattern 27.5 (barLen barOpts) melody
    -- Цикл в hsig это секунда, такт растягиваем до двух: восемь событий в
    -- секунду ухо в структуру не складывает.
    barSec = 2
    total = 2 * fromIntegral (length bs)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let n = case args of
        (a : _) | [(k, "")] <- reads a -> k
        _ -> 12
      bs = bars n
      (states, syms) = traceOf bs
      -- Один корень имени на прогон: звук и картинки к нему складываются
      -- вместе и длинный прогон не затирает картинки калибровочного.
      stem' = "out/" <> (if n == 12 then "pred" else "pred-" <> show n)
  printf "тактов %d, длительность %d с\n" (length bs) (2 * length bs)
  printf "h_mu   = %.4f бит на символ\n" (entropyRate ring)
  printf "C_mu   = %.4f бит\n" (statComplexity ring)
  printf "липшиц = %.3f, искажение = %.3f\n" lip dist'
  -- Картинки кладутся рядом со звуком и тем же прогоном: схема, разошедшаяся
  -- с машиной, врёт про то, что звучит, а сверять их руками никто не станет.
  writeFile (stem' <> "-kernel.mmd") (mermaidOf ring stateLabel symLabel 0.02)
  writeFile (stem' <> "-kernel.svg") (ringSvg defaultTheme ring semisLabel 0.05)
  writeFile (stem' <> "-trace.svg") (traceSvg defaultTheme ring (barLen barOpts) 20 states)
  printf "картинки: %s-kernel.svg, %s-trace.svg, %s-kernel.mmd\n" stem' stem' stem'
  -- Сравнение режимов отбора считает четыре пьесы, поэтому только на
  -- калибровочной длине. На длинном прогоне интересна одна кривая.
  if n <= 16
    then do
      putStrLn "ошибка модели по тактам, четыре режима отбора:"
      printf "  умолчание   %s\n" (curve bs)
      printf "  без порога  %s\n" (curve noTypical)
      printf "  с окном     %s\n" (curve withWindow)
      printf "  без выбора  %s\n" (curve baseline)
    else do
      putStrLn "ошибка модели, каждый восьмой такт:"
      printf "  %s\n" (curve (everyNth 8 bs))
  putStrLn "--- ядро ---"
  printf "палитра в полутонах: %s\n" (show [map (`mod` 12) (chordSemis tonality c) | c <- stateChords])
  printf "состояния: %s\n" (concatMap show (take 200 states))
  printf "символы:   %s\n" (concatMap show (take 200 syms))
  printf
    "ядро %.0f бит; поток %d событий, из них неустранимых %.0f бит\n"
    kernelBits
    (length syms)
    (fromIntegral (length syms) * entropyRate ring)
  printf
    "нот в партитуре: гармония %d из %d слотов, мелодия %d из %d\n"
    (3 * countRuns (fst (linesOf bs)))
    (3 * length syms)
    (countRuns (snd (linesOf bs)))
    (length syms)
  renderTrack defaultEnv (stem' <> ".wav") (track bs) >>= putStrLn
  where
    stateLabel s = show s <> ": " <> show (semis s)
    semisLabel = unwords . map show . semis
    semis s = map (`mod` 12) (chordSemis tonality (chordOf s))
    symLabel x = ["стоять", "вперёд", "назад", "через одну"] !! x
    everyNth k xs = [x | (i, x) <- zip [0 :: Int ..] xs, i `mod` k == 0]
    -- Размер описания ядра: таблица сдвигов, разбиение состояний на три
    -- класса поведения и сами распределения с точностью до сотой.
    kernelBits :: Double
    kernelBits =
      4 * logBase 2 6 -- delta: четыре значения по модулю шесть
        + 6 * logBase 2 3 -- какое состояние к какому классу
        + 9 * logBase 2 100 -- три строки по три свободных веса
    countRuns evs = sum [length (runsOf bar) | bar <- chunksOf (barLen barOpts) evs]
    curve bs = unwords [printf "%5.2f" (barError b) :: String | b <- bs]
    d = distMatrix (map (\s -> unfoldPred (machineOut ring) (machineStep ring) s) (machineStates ring))
    lip = lipschitz tonality stateChords d
    dist' = distortion tonality stateChords d
