-- | Первая пьеса: предиктивная модель, уложенная в аккорды и сыгранная.
--
-- Что здесь проверяется на слух, а не тестом: слышна ли статистическая
-- сложность. Гармония несёт причинное состояние машины, мелодия несёт
-- излучённый символ. Состояния, предсказывающие похоже, уложены рядом по
-- голосоведению, поэтому близость в модели обязана звучать как близость
-- гармонии.
module Main (main) where

import Sound.Pred.Compose
import Sound.Pred.Dist qualified as D
import Sound.Pred.Machine
import Sound.Pred.Metric (distMatrix)
import Sound.Pred.Model (unfoldPred)
import Sound.Pred.Orbifold
import Sound.Pred.Render
import Data.Function ((&))
import Sound.Sig
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

bars :: [Bar Int Int]
bars = compose barOpts ring chordOf tonality alphabet 12

-- | Контроль: тот же процесс без всякого выбора, один кандидат на такт.
--
-- Нужен, чтобы отличить обучение слушателя от работы жадного поиска. Если
-- ошибка модели у контроля падает, а у поиска растёт, виноват отбор, а не
-- слушатель.
baseline :: [Bar Int Int]
baseline = compose barOpts {barCands = 1} ring chordOf tonality alphabet 12

-- | Гармония по событиям и мелодия из символов.
--
-- Мелодия берёт голос по номеру символа и поднимает его на октаву: символ
-- обязан быть слышен отдельно от состояния, иначе слушателю нечего
-- моделировать.
lines' :: ([[Double]], [Double])
lines' = (voices, melody)
  where
    states = concatMap barStates bars
    syms = concatMap barSyms bars
    voices = voiceLines tonality 36 (map chordOf states)
    melody = [pick s vs | (s, vs) <- zip syms voices]
    pick s vs = vs !! (s `mod` length vs) + 12

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

track :: [Stem]
track =
  [ stem "pred-harm" (takeSec total (play pad (slow barSec harm)))
  , stem "pred-mel" (takeSec total (play pluck (slow barSec mel)))
  ]
  where
    (voices, melody) = lines'
    harm = harmonyPattern 27.5 (barLen barOpts) voices
    mel = melodyPattern 27.5 (barLen barOpts) melody
    -- Цикл в hsig это секунда, такт растягиваем до двух: восемь событий в
    -- секунду ухо в структуру не складывает.
    barSec = 2
    total = 2 * fromIntegral (length bars)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  printf "h_mu   = %.4f бит на символ\n" (entropyRate ring)
  printf "C_mu   = %.4f бит\n" (statComplexity ring)
  printf "аккорды: %s\n" (show (map chordDegrees stateChords))
  printf "липшиц = %.3f, искажение = %.3f\n" lip dist'
  putStrLn "такт  ошибка  выигрыш  сюрприз  ожидал  скачок  в окне"
  mapM_ report (zip [1 :: Int ..] bars)
  putStrLn "контроль без выбора: ошибка по тактам"
  putStrLn (unwords [printf "%.2f" (barError b) | b <- baseline])
  renderTrack defaultEnv "out/pred.wav" track >>= putStrLn
  where
    d = distMatrix (map (\s -> unfoldPred (machineOut ring) (machineStep ring) s) (machineStates ring))
    lip = lipschitz tonality stateChords d
    dist' = distortion tonality stateChords d
    report (i, b) =
      printf
        "%4d  %6.3f  %7.4f  %7.3f  %6.3f  %6.1f  %s\n"
        i
        (barError b)
        (barGain b)
        (barSurprisal b)
        (barExpected b)
        (barLeap b)
        (if barFeasible b then "да" else "НЕТ")
