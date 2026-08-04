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
import Sound.Pred.Listener
import Sound.Pred.Machine
import Sound.Pred.Metric (defaultGamma, distMatrix, distMatrixWith)
import Sound.Pred.Model (unfoldPred)
import Sound.Pred.Orbifold
import Sound.Pred.Render
import Sound.Sig
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))
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

-- | Машина с периодом в такт: фраза объявляет себя и потом себя развивает.
--
-- Состояние это доля такта и наклонение блока. На нулевой доле выход
-- равномерен, и излучённый символ задаёт наклонение на всю фразу: это
-- объявление стоит ровно два бита. Дальше семь долей идут из острого
-- распределения выбранного наклонения и стоят около восьми десятых бита.
--
-- Зачем так. У стационарного процесса все доли такта статистически
-- одинаковы, и никакой профиль сюрприза из него не выжать: пик на границе
-- пришлось бы навязывать отбором, то есть врать про процесс. Пик обязан
-- быть свойством материала, и тогда он честен. Машина остаётся унифилярной:
-- наклонение читается из объявляющего символа.
phrased :: Machine (Int, Int) Int
phrased =
  Machine
    { machineStart = (0, 0)
    , machineStates = (0, 0) : [(p, m) | p <- [1 .. 7], m <- [0, 1]]
    , machineOut = \(p, m) ->
        if p == 0
          then D.uniform alphabet
          else D.dist (if m == 0 then [(0, 0.85), (1, 0.1), (2, 0.03), (3, 0.02)] else [(2, 0.85), (3, 0.1), (0, 0.03), (1, 0.02)])
    , machineStep = \(p, m) x ->
        if p == 0
          then (1, x `mod` 2)
          else if p == 7 then (0, 0) else (p + 1, m)
    }

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
-- | Гармония несёт только то, чего нет в метре.
--
-- Состояние фразовой машины склеено из двух независимых вещей: доли такта
-- и наклонения фразы. Долю уже несёт сам метр, и дублировать её гармонией
-- незачем - а попытка это сделать проваливается измеримо: пятнадцать
-- состояний не укладываются в трёхголосие без совпадений, и искажение
-- уходит в бесконечность.
--
-- Поэтому укладывается фактор: объявление фразы и два наклонения. Три
-- точки, три аккорда, конечное искажение. Граница такта получает
-- собственную гармонию, и это ровно та гармоническая ритмика, что принята
-- в музыке: аккорд на такт, смена на сильной доле.
phrasedDist :: [[Double]]
phrasedDist = distMatrixWith 9 defaultGamma (map asPred [(0, 0), (4, 0), (4, 1)])
  where
    asPred s = unfoldPred (machineOut phrased) (machineStep phrased) s

phrasedChords :: [Chord]
phrasedChords = embed opts phrasedDist
  where
    opts = defaultEmbed {embedScale = tonality, embedVoices = 3, embedSpan = 5}

phrasedChord :: (Int, Int) -> Chord
phrasedChord (p, m) = phrasedChords !! (if p == 0 then 0 else 1 + m)

-- Пьеса ------------------------------------------------------------------------

barOpts :: BarOpts
barOpts = defaultBarOpts {barLen = 8, barCands = 32, barVlMax = 5, barOrder = 3}

-- | Сколько тактов сочинять. Двенадцать это калибровочный прогон, на нём
-- считаются сравнения режимов отбора; длиннее задаётся аргументом.
barCount :: Int -> Int
barCount n = max 1 n

bars :: Int -> [Bar (Int, Int) Int]
bars n = compose barOpts phrased phrasedChord tonality alphabet (barCount n)

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

-- | Без краткосрочной памяти: слушатель помнит всё и ничего не забывает на
-- границах фраз.
noShort :: [Bar Int Int]
noShort = compose barOpts {barShortOrder = 0} ring chordOf tonality alphabet 12

-- | Средний сюрприз по позиции внутри такта.
--
-- Пик на нулевой позиции означает, что граница такта чем-то отмечена и
-- слушателю есть за что зацепиться. Плоский профиль означает, что такт для
-- него не существует.
--
-- Числа на позицию усредняются по всем тактам, поэтому на коротком прогоне
-- это шум: при двенадцати тактах ошибка среднего около половины бита, и
-- «пик» может оказаться где угодно. Мерить надо на сотнях.
profileOf :: Int -> [Bar s Int] -> [Double]
profileOf shortOrder bs =
  [ mean [ss !! (b * len + p) | b <- [0 .. length bs - 1]]
  | p <- [0 .. len - 1]
  ]
  where
    len = barLen barOpts
    ss = onlineSurprisalsSeg (newListenerWith (barOrder barOpts) shortOrder alphabet) (map barSyms bs)
    mean xs = sum xs / fromIntegral (length xs)

-- | Отношение сюрприза на границе такта к среднему по остальным долям.
-- Единица это плоский профиль, больше единицы - слышимая граница.
frontRatio :: [Double] -> Double
frontRatio [] = 1
frontRatio (p0 : rest)
  | null rest || tailMean' <= 0 = 1
  | otherwise = p0 / tailMean'
  where
    tailMean' = sum rest / fromIntegral (length rest)

baseline :: [Bar Int Int]
baseline = compose barOpts {barCands = 1} ring chordOf tonality alphabet 12

-- | След пьесы: причинное состояние перед каждым событием и сам символ.
-- Это всё содержание трека; ниже из него выводится любая слышимая деталь.
traceOf :: [Bar s Int] -> ([s], [Int])
traceOf bs = (concatMap barStates bs, concatMap barSyms bs)

-- | Гармония по состояниям и мелодия по символам.
--
-- Два независимых канала: гармония несёт причинное состояние, мелодия
-- несёт излучённый символ. Смешивать их нельзя, иначе слушателю не из чего
-- разделить «где мы» и «что произошло».
linesOf :: (s -> Chord) -> [Bar s Int] -> ([[Double]], [Double])
linesOf chordFor bs = (voices, melody)
  where
    (states, syms) = traceOf bs
    voices = voiceLines tonality 36 (map chordFor states)
    -- Символ выбирает ступень над основанием аккорда, а не номер голоса.
    -- При трёх голосах и четырёх символах отображение в номер склеивало бы
    -- нулевой символ с третьим, то есть теряло бы информацию ровно там,
    -- где её надо передать.
    melody = degreeLine tonality 55 [root st + 2 * s | (st, s) <- zip states syms]
    root st = minimum (chordDegrees (chordFor st))

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

-- | Стемы прогона. Корень имени приходит снаружи: два прогона рядом не
-- должны спорить за одни и те же промежуточные файлы.
track :: (s -> Chord) -> String -> [Bar s Int] -> [Stem]
track chordFor name bs =
  [ stem (name <> "-harm") (takeSec total (play pad (slow barSec harm)))
  , stem (name <> "-mel") (takeSec total (play pluck (slow barSec mel)))
  ]
  where
    (voices, melody) = linesOf chordFor bs
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
      -- Второй аргумент "noaudio" пропускает рендер. Профиль сюрприза и
      -- кривые ошибки считаются из символов, а звук на длинном прогоне
      -- занимает минуты: для замеров он лишний.
      quiet = "noaudio" `elem` args
      -- Та же длина, та же оснастка, другая машина: сравнивать профили
      -- имеет смысл только при прочих равных.
      ringBars = compose barOpts ring chordOf tonality alphabet (barCount n)
      bs = bars n
      ringProfile = profileOf 0 ringBars
      barProfile = profileOf 0 bs
      (states, syms) = traceOf bs
      -- Один корень имени на прогон: звук, стемы и картинки к нему
      -- складываются вместе, и длинный прогон не затирает калибровочный.
      name = if n == 12 then "pred" else "pred-" <> show n
      path suffix = "out" </> name <> suffix
  -- Каталог создаётся до записи: картинки пишутся раньше рендера, и вне
  -- репозитория (nix run) out/ ещё нет.
  createDirectoryIfMissing True "out"
  printf "тактов %d, длительность %d с\n" (length bs) (2 * length bs)
  printf "h_mu   = %.4f бит на символ\n" (entropyRate phrased)
  printf "C_mu   = %.4f бит\n" (statComplexity phrased)
  printf "липшиц = %.3f, искажение = %.3f\n" lip dist'
  -- Картинки кладутся рядом со звуком и тем же прогоном: схема, разошедшаяся
  -- с машиной, врёт про то, что звучит, а сверять их руками никто не станет.
  writeFile (path "-kernel.mmd") (mermaidOf phrased stateLabel symLabel 0.02)
  writeFile (path "-kernel.svg") (ringSvg defaultTheme phrased semisLabel 0.05)
  writeFile (path "-trace.svg") (traceSvg defaultTheme phrased (barLen barOpts) 20 states)
  printf "картинки: %s-kernel.svg, %s-trace.svg, %s-kernel.mmd\n" (path "") (path "") (path "")
  -- Сравнение режимов отбора считает четыре пьесы, поэтому только на
  -- калибровочной длине. На длинном прогоне интересна одна кривая.
  if n <= 16
    then do
      putStrLn "ошибка модели по тактам, режимы отбора (на кольцевой машине):"
      printf "  умолчание   %s\n" (curve ringBars)
      printf "  без краткой %s\n" (curve noShort)
      printf "  без порога  %s\n" (curve noTypical)
      printf "  с окном     %s\n" (curve withWindow)
      printf "  без выбора  %s\n" (curve baseline)
      putStrLn "ошибка модели фразовой машины, по тактам:"
      printf "  %s\n" (curve bs)
    else do
      putStrLn "ошибка модели, каждый восьмой такт:"
      printf "  %s\n" (curve (everyNth 8 bs))
  putStrLn "--- ядро ---"
  printf "палитра в полутонах: %s\n" (show [map (`mod` 12) (chordSemis tonality c) | c <- phrasedChords])
  printf "доли:      %s\n" (concatMap (show . fst) (take 200 states))
  printf "символы:   %s\n" (concatMap show (take 200 syms))
  printf
    "ядро %.0f бит; поток %d событий, из них неустранимых %.0f бит\n"
    kernelBits
    (length syms)
    (fromIntegral (length syms) * entropyRate phrased)
  printf
    "нот в партитуре: гармония %d из %d слотов, мелодия %d из %d\n"
    (3 * countRuns (fst (linesOf phrasedChord bs)))
    (3 * length syms)
    (countRuns (snd (linesOf phrasedChord bs)))
    (length syms)
  putStrLn "сюрприз по позиции внутри такта, бит:"
  printf "  кольцо      %s   граница/остальные %.2f\n" (row ringProfile) (frontRatio ringProfile)
  printf "  фразовая    %s   граница/остальные %.2f\n" (row barProfile) (frontRatio barProfile)
  if quiet
    then putStrLn "рендер пропущен"
    else renderTrack defaultEnv (path ".wav") (track phrasedChord name bs) >>= putStrLn
  where
    stateLabel (p, m) = show p <> "." <> show m
    semisLabel = unwords . map show . semis
    semis s = map (`mod` 12) (chordSemis tonality (phrasedChord s))
    symLabel x = show x
    everyNth k xs = [x | (i, x) <- zip [0 :: Int ..] xs, i `mod` k == 0]
    -- Размер описания фразовой машины: длина такта, число наклонений, два
    -- распределения по три свободных веса с точностью до сотой.
    kernelBits :: Double
    kernelBits =
      logBase 2 16 -- длина такта
        + logBase 2 4 -- сколько наклонений
        + 6 * logBase 2 100 -- два распределения по три свободных веса
    countRuns evs = sum [length (runsOf bar) | bar <- chunksOf (barLen barOpts) evs]
    curve bs = unwords [printf "%5.2f" (barError b) :: String | b <- bs]
    row vs = unwords [printf "%5.2f" v :: String | v <- vs]
    lip = lipschitz tonality phrasedChords phrasedDist
    dist' = distortion tonality phrasedChords phrasedDist
