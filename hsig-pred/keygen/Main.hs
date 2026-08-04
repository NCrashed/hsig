-- | Пьеса в духе Keygen Church: барочный орган поверх чиптюна.
--
-- Стиль взят как ограничение, а не как украшение. Он диктует, какое ядро
-- имеет смысл: барокко это функциональная гармония, то есть процесс, где
-- ступень определяется тем, куда она разрешается. Чиптюн это арпеджио, то
-- есть почти детерминированная фигура внутри такта. Обе вещи ложатся в одну
-- машину, и обе она честно несёт.
--
-- Ядро сложнее прежнего вдвое: пятьдесят шесть причинных состояний против
-- пятнадцати. Слышимого материала при этом не прибавилось - прибавилось
-- структуры.
module Main (main) where

import Data.Function ((&))
import Sound.Pred.Compose
import Sound.Pred.Diagram
import Sound.Pred.Dist qualified as D
import Sound.Pred.Listener
import Sound.Pred.Machine
import Sound.Pred.Metric (defaultGamma, distMatrixWith)
import Sound.Pred.Model (unfoldPred)
import Sound.Pred.Orbifold
import Sound.Pred.Render
import Sound.Sig
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Printf (printf)

-- Ядро --------------------------------------------------------------------

-- | Событий в такте. Восьмые при темпе ниже: такт это половина такта 4/4.
beats :: Int
beats = 8

-- | Символ это ступень лада, а два последних - объявление смены режима.
--
-- На сильной доле символ 0..6 называет новую ступень, а 7 и 8 говорят
-- «каденция в другой режим». На остальных долях символ берёт ноту арпеджио,
-- и 7 с 8 там не встречаются.
alphabet :: [Int]
alphabet = [0 .. 8]

-- | Сколько гармонических режимов.
regimes :: Int
regimes = 3

-- | Стационарная модель исчерпывается: на этой машине слушатель выучивал
-- её за восемь секунд, дальше кривая ошибки ползла (docs/PRED.md, разд. про
-- нестационарность). У стационарного процесса избыточная энтропия конечна,
-- и трёхминутная пьеса из него невозможна не по недоработке, а потому что
-- передавать больше нечего.
--
-- Поэтому машин три, и переход между ними сам несёт информацию. Режимы
-- отличаются и функциональной гармонией, и фигурой арпеджио: слушателю
-- приходится переучивать оба слоя.
resolvesIn :: Int -> Int -> [(Int, Double)]
resolvesIn 0 d = case d of
  -- Классический минор: доминанта разрешается почти наверняка.
  0 -> [(3, 0.30), (4, 0.30), (5, 0.20), (0, 0.20)]
  1 -> [(4, 0.70), (0, 0.15), (3, 0.15)]
  2 -> [(5, 0.50), (3, 0.30), (6, 0.20)]
  3 -> [(4, 0.60), (0, 0.20), (1, 0.20)]
  4 -> [(0, 0.80), (5, 0.15), (4, 0.05)]
  5 -> [(1, 0.40), (3, 0.30), (4, 0.30)]
  _ -> [(2, 0.60), (0, 0.25), (4, 0.15)]
resolvesIn 1 d = case d of
  -- Плагальный: доминанты почти нет, ход по квартам, VII вместо V.
  0 -> [(3, 0.45), (6, 0.30), (5, 0.15), (0, 0.10)]
  1 -> [(3, 0.55), (6, 0.25), (0, 0.20)]
  2 -> [(5, 0.45), (1, 0.35), (3, 0.20)]
  3 -> [(0, 0.50), (6, 0.30), (5, 0.20)]
  4 -> [(3, 0.45), (0, 0.35), (6, 0.20)]
  5 -> [(3, 0.40), (2, 0.35), (1, 0.25)]
  _ -> [(0, 0.55), (3, 0.30), (2, 0.15)]
resolvesIn _ d =
  -- Секвенция по квинтам вниз: внутри себя почти детерминирована, но это
  -- совсем другая цепь, и выученное в других режимах тут не помогает.
  [((d + 3) `mod` 7, 0.82), ((d + 1) `mod` 7, 0.12), (d, 0.06)]

-- | Полная вероятность ухода в другой режим с этой ступени.
--
-- Уйти можно только с доминанты: смена тональности через каденцию, как это
-- и делается. Заодно это ставит смену на слышимое место - слушатель уже
-- ждёт разрешения, и вместо него приходит поворот.
switchFrom :: Int -> Int -> Double
switchFrom r d
  | d /= dominantOf r = 0
  | otherwise = 0.34
  where
    dominantOf 1 = 6 -- в плагальном роль доминанты у VII
    dominantOf _ = 4

-- | Распределение символов на сильной доле: ступени плюс два объявления.
announce :: Int -> Int -> [(Int, Double)]
announce r d =
  [(x, w * (1 - sw)) | (x, w) <- resolvesIn r d]
    <> (if sw > 0 then [(7, sw / 2), (8, sw / 2)] else [])
  where
    sw = switchFrom r d

-- | Фигура арпеджио по долям, своя у каждого режима.
--
-- Почти детерминированная, и это тоже стиль: чиптюновое арпеджио не
-- случайная последовательность, а узнаваемая фигура. Отсюда же берётся
-- профиль сюрприза - доли дёшевы, сильная доля дорога. Разные фигуры по
-- режимам означают, что при смене переучивать приходится и ритм.
figure :: Int -> Int -> Int
figure r p = table !! (p `mod` beats)
  where
    table = case r of
      0 -> [0, 0, 2, 4, 2, 0, 4, 2]
      1 -> [0, 4, 2, 4, 0, 2, 4, 2]
      _ -> [0, 2, 4, 6, 4, 2, 0, 2]

arpeggio :: Int -> Int -> [(Int, Double)]
arpeggio r p = (figure r p, 0.85) : rest
  where
    rest = [(x, w) | (x, w) <- spread, x /= figure r p]
    spread = [(0, 0.05), (2, 0.05), (4, 0.05), (6, 0.03), (1, 0.01), (3, 0.01), (5, 0.01)]

-- | Состояние: режим, доля такта и ступень.
--
-- Машина унифилярна: и новая ступень, и новый режим читаются прямо из
-- объявляющего символа.
church :: Machine (Int, Int, Int) Int
church =
  Machine
    { machineStart = (0, 0, 0)
    , machineStates = [(r, p, d) | r <- [0 .. regimes - 1], p <- [0 .. beats - 1], d <- [0 .. 6]]
    , machineOut = \(r, p, d) -> D.dist (if p == 0 then announce r d else arpeggio r p)
    , machineStep = \(r, p, d) x ->
        if p /= 0
          then (r, (p + 1) `mod` beats, d)
          else case x of
            -- Смена режима приземляется на тонику: каденция обязана куда-то
            -- разрешиться, иначе поворот не слышен как поворот.
            7 -> ((r + 1) `mod` regimes, 1, 0)
            8 -> ((r + 2) `mod` regimes, 1, 0)
            _ -> (r, 1, if x <= 6 then x else d)
    }

-- | Кто играет в этом режиме. Состав инструментов это рендер верхнего
-- состояния, а не украшение: смена раздела отмечается тем, кто вступил и
-- кто ушёл, и это самый быстро опознаваемый признак из всех.
plays :: Int -> String -> Bool
plays r part = case (r, part) of
  (1, "chip") -> False
  (1, "saws") -> False
  (2, "organ") -> False
  _ -> True

-- Гармония ------------------------------------------------------------------

tonality :: Scale
tonality = mkScale "harmonicMinor"

-- | Укладываются ступени, а не состояния.
--
-- Долю такта уже несёт метр (docs/PRED.md, разд. про профиль сюрприза), и
-- дублировать её гармонией незачем. Представителями берутся состояния на
-- сильной доле: они различаются выходом сразу, поэтому хватает малой
-- глубины метрики.
degreeDist :: [[Double]]
degreeDist = distMatrixWith 5 defaultGamma [asPred (0, 0, d) | d <- [0 .. 6]]
  where
    asPred s = unfoldPred (machineOut church) (machineStep church) s

degreeChords :: [Chord]
degreeChords = embed opts degreeDist
  where
    opts = defaultEmbed {embedScale = tonality, embedSpan = 5}

-- | Аккорд состояния: ступень, сдвинутая по режиму.
--
-- Укладываются семь ступеней одного режима, а режим добавляет
-- диатонический перенос. Так и должно быть: перенос это точная симметрия
-- пространства ступеней ('transposeDeg'), поэтому режим слышится сменой
-- тонального центра, а не новой палитрой. Укладывать двадцать одно
-- состояние в трёхголосие всё равно нечем - консонантных трезвучий в ладу
-- ровно семь.
chordOf :: (Int, Int, Int) -> Chord
chordOf (r, _, d) = transposeDeg (shift r) (degreeChords !! d)
  where
    shift 1 = 3
    shift 2 = 5
    shift _ = 0

-- Пьеса ---------------------------------------------------------------------

barOpts :: BarOpts
barOpts = defaultBarOpts {barLen = beats, barCands = 32, barVlMax = 6, barOrder = 3}

bars :: Int -> [Bar (Int, Int, Int) Int]
bars n = compose barOpts church chordOf tonality alphabet (max 1 n)

-- | Ноты по партиям, с пропусками там, где режим партию не пускает.
--
-- Голоса ведутся насквозь, а гасятся уже после: голосоведение не должно
-- рваться оттого, что партия помолчала - вернувшись, она обязана
-- продолжить с той же высоты, а не начать заново.
linesOf ::
  [Bar (Int, Int, Int) Int] ->
  ([Maybe [Double]], [Maybe Double], [Double], [Maybe [Double]])
linesOf bs = (onlyIn "organ" voices, onlyIn "chip" lead, bassLine, onlyIn "saws" sawVoices)
  where
    states = concatMap barStates bs
    syms = concatMap barSyms bs
    regimeOf (r, _, _) = r
    onlyIn part xs = [if plays (regimeOf st) part then Just x else Nothing | (st, x) <- zip states xs]

    voices = voiceLines tonality 35 (map chordOf states)
    -- Символ это ступень лада: мелодия читается напрямую, без переводов.
    -- Символы 7 и 8 объявляют смену режима и звучат скачком вверх - это
    -- ровно то место, где происходит поворот, и слышать его надо.
    lead = degreeLine tonality 64 [root st + s | (st, s) <- zip states syms]
    bassLine = degreeLine tonality 16 [root st | st <- states]
    -- Основание и квинта, регистр между басом и органом. Полудиапазон
    -- узкий: партия второго плана не должна разъезжаться по регистру,
    -- иначе она перестаёт быть подложкой и начинает спорить с органом.
    sawVoices = voiceLinesIn tonality 22 6 [mkChord [root st, root st + 4] | st <- states]
    root st = minimum (chordDegrees (chordOf st))

-- Голоса ----------------------------------------------------------------------

-- | Регистр органа: набор частичных с весами, ровно как стопы.
--
-- Аддитивный синтез это и есть регистровка, поэтому орган тут получается
-- не имитацией, а по определению. Взяты 8', 4', 2 2/3', 2', 1 1/3' и 1'.
pipes :: [(Double, Double)]
pipes = [(1, 1), (2, 0.5), (3, 0.22), (4, 0.18), (6, 0.06), (8, 0.04)]

organ :: Instrument
organ n =
  sum [constant g * sine (constant (noteFreq n * r)) | (r, g) <- pipes]
    * 0.07
    -- У органа нет затухания: клапан открыт, пока нажата клавиша.
    & (* adsr 0.012 0.03 0.92 0.09 (noteDur n))


-- | Ведущий голос: импульс с узкой скважностью, битый по разрядности.
--
-- decimate это и есть та самая восьмибитность: не фильтр «под старину», а
-- честное огрубление разрядности и частоты.
chip :: Instrument
chip n =
  pulse 0.22 (constant (noteFreq n))
    * constant (0.22 * noteAmp n)
    & ladder (2200 + 5000 * expdecay 0.05) 0.5
    & decimate 6 3
    & (* adsr 0.002 0.05 0.35 0.05 (noteDur n))

-- | Пила с ограниченным числом частичных.
--
-- Обрезание ряда это не аппроксимация в смысле DESIGN.md, разд. 2: там
-- запрещены naive saw и PolyBLEP, потому что они дают алиасинг. Обрезанный
-- ряд не даёт его вовсе - это ровно та же пила, только тёмная, и всё, что
-- отброшено, ладдер срезал бы следом. Взамен партия второго плана стоит
-- десятков осцилляторов на ноту вместо сотен.
sawPipes :: Int -> Double -> Sig
sawPipes parts f =
  sum
    [ constant (1 / fromIntegral k) * sine (constant (f * d * fromIntegral k))
    | d <- [0.994, 1.006]
    , k <- [1 .. parts]
    ]

-- | Пилы второго плана: расстроенная пара на основании и квинте.
--
-- Информации не несёт: доля такта, ступень и символ уже разобраны метром,
-- органом и ведущим голосом. Это оркестровка, и выдавать её за носитель
-- было бы враньём. Её работа - тяга и плотность в середине, там где орган
-- держит, а бас только отмечает.
saws :: Instrument
saws n =
  sawPipes 12 (noteFreq n)
    * constant (0.28 * noteAmp n)
    -- Срез выше прежнего не для яркости, а против маскировки. При потолке
    -- в килогерц вся энергия пил лежала там же, где бас и основание
    -- органа, и партия пропадала не от тихости, а оттого что её нечем
    -- было услышать. Полоса до двух килогерц у неё своя.
    & ladder (520 + 1500 * expdecay 0.3) 0.5
    & (* adsr 0.006 0.09 0.55 0.07 (noteDur n))

-- | Бас: четыре частичных и жёсткое ограничение сверху.
bass :: Instrument
bass n =
  sum [constant g * sine (constant (noteFreq n * r)) | (r, g) <- [(1, 1), (2, 0.5), (3, 0.22), (4, 0.1)]]
    * constant (0.22 * noteAmp n)
    & clip 0.5
    & (* adsr 0.004 0.06 0.6 0.05 (noteDur n))

-- | Барабаны стоят вне машины намеренно: метрическая сетка задаётся
-- снаружи и из материала не выводится (docs/PRED.md, разд. 2).
drums :: Instrument
drums n = case noteLabel n of
  "bd" -> sine (constant 55 * (1 + 7 * expdecay 0.03)) * adsr 0.001 0.12 0 0.02 0.14 * 0.5
  "sd" -> (noise 7 * 0.6 + sine (constant 190) * 0.4) & highpass 320 & (* adsr 0.001 0.11 0 0.03 0.13) & (* 0.34)
  _ -> noise 11 & highpass 7000 & (* adsr 0.001 0.028 0 0.01 0.035) & (* 0.16)

-- | Реверберация нефа: четыре гребёнки и два фазовых звена.
nave :: Fx
nave x = (x * 0.72 + wet * 0.34) & allpass 0.0071 0.62 & allpass 0.0113 0.58
  where
    -- Гребёнка с обратной связью 0.72 имеет установившееся усиление около
    -- 3.6, четыре штуки дают 14. Множитель 0.07 возвращает влажный тракт к
    -- единице: без него орган выходил за предел и мастер клипповал.
    wet = sum [comb t 0.72 x | t <- [0.0297, 0.0371, 0.0411, 0.0437]] * 0.07

track :: String -> [Bar (Int, Int, Int) Int] -> [Stem]
track name bs =
  [ stem (name <> "-organ") (takeSec total (play organ (slow barSec harm) & nave))
  , stem (name <> "-chip") (takeSec total (play chip (slow barSec lead') & sidechain kickSig 0.4))
  , -- Пилы разведены по краям образа: в центре и без них тесно, там орган,
    -- бас и бочка.
    panned (-0.45) (stem (name <> "-sawL") (takeSec total (play saws (slow barSec sawPatL) & sidechain kickSig 0.55)))
  , panned 0.45 (stem (name <> "-sawR") (takeSec total (play saws (slow barSec sawPatR) & sidechain kickSig 0.55)))
  , stem (name <> "-bass") (takeSec total (play bass (slow barSec bassPat) & sidechain kickSig 0.6))
  , stem (name <> "-drums") (takeSec total (play drums (slow barSec drumPat)))
  ]
  where
    (voices, lead, bassLine, sawLines) = linesOf bs
    harm = harmonyGated 27.5 beats voices
    lead' = accentGated 27.5 0.45 beats lead
    bassPat = accentPattern 27.5 0.6 beats bassLine
    -- Голоса пил разнесены по стемам, а не сложены в один: панорама у них
    -- разная, а стем несёт одну панораму.
    sawPatL = accentGatedVoice 0 sawLines
    sawPatR = accentGatedVoice 1 sawLines
    -- Бочка на первой и пятой доле, малый на пятой, хэт на каждой.
    drumPat = notes "[bd hh] hh [bd hh] hh [sd hh] hh hh [hh hh]"
    -- share обязателен: сигнал используется четырьмя стемами, и без него
    -- он считается заново на каждый (DESIGN.md, разд. 3).
    kickSig = share (play drums (slow barSec (notes "bd ~ bd ~ ~ ~ ~ ~")))
    -- Такт это восемь восьмых при 150 ударах в минуту.
    -- Time рациональное, а takeSec берёт Double: одно и то же число нужно
    -- в двух типах, и молча его не привести.
    barSec = 8 / 5 :: Time
    barSecs = 1.6 :: Double
    total = barSecs * fromIntegral (length bs)

-- Диагностика -----------------------------------------------------------------

profileOf :: [Bar s Int] -> [Double]
profileOf bs =
  [ mean [ss !! (b * beats + p) | b <- [0 .. length bs - 1]]
  | p <- [0 .. beats - 1]
  ]
  where
    ss = onlineSurprisalsSeg (newListenerWith (barOrder barOpts) 0 alphabet) (map barSyms bs)
    mean xs = sum xs / fromIntegral (length xs)

frontRatio :: [Double] -> Double
frontRatio (p0 : rest)
  | not (null rest) && back > 0 = p0 / back
  where
    back = sum rest / fromIntegral (length rest)
frontRatio _ = 1

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let n = case args of
        (a : _) | [(k, "")] <- reads a -> k
        _ -> 24
      quiet = "noaudio" `elem` args
      bs = bars n
      states = concatMap barStates bs

      name = "keygen-" <> show n
      (hv, hl, hb, hs) = linesOf bs
      path suffix = "out" </> name <> suffix
  createDirectoryIfMissing True "out"
  printf "тактов %d, длительность %.1f с\n" (length bs) (1.6 * fromIntegral (length bs) :: Double)
  printf "состояний %d, h_mu = %.4f бит, C_mu = %.4f бит\n" (length (machineStates church)) (entropyRate church) (statComplexity church)
  printf "липшиц = %.3f, искажение = %.3f\n" (lipschitz tonality degreeChords degreeDist) (distortion tonality degreeChords degreeDist)
  printf "ступени в полутонах: %s\n" (show [map (`mod` 12) (chordSemis tonality c) | c <- degreeChords])
  writeFile (path "-kernel.svg") (ringSvg defaultTheme church (const "") 0.06)
  writeFile (path "-trace.svg") (traceSvg defaultTheme church beats 16 states)
  printf "картинки: %s-kernel.svg, %s-trace.svg\n" (path "") (path "")
  printf "ошибка по тактам: %s\n" (unwords [printf "%.2f" (barError b) :: String | b <- bs])
  printf "режимы по тактам:  %s\n" (concatMap (show . reg . head' . barStates) bs)
  printf "ступени по тактам: %s\n" (concatMap (show . deg . head' . barStates) bs)
  printf "сюрприз по доле: %s   граница/остальные %.2f\n" (unwords [printf "%5.2f" v :: String | v <- profileOf bs]) (frontRatio (profileOf bs))
  printf "сюрприз по тактам (с краткой памятью): %s\n" (unwords [printf "%.1f" v :: String | v <- perBar bs])
  printf "нот: гармония %d, ведущий %d, бас %d, пилы %d\n" (3 * runs hv) (runs hl) (runs hb) (2 * runs hs)
  if quiet
    then putStrLn "рендер пропущен"
    else renderTrack defaultEnv (path ".wav") (track name bs) >>= putStrLn
  where
    reg (r, _, _) = r
    deg (_, _, d) = d
    head' xs = case xs of
      (x : _) -> x
      [] -> error "keygen: такт без состояний"
    runs :: (Eq a) => [a] -> Int
    runs evs = sum [length (runsOf bar) | bar <- chunksOf beats evs]


-- | Один голос из многоголосной партии с пропусками.
--
-- Пилы разнесены по стемам, потому что панорама у них разная, а стем несёт
-- одну панораму. Молчание режима при этом сохраняется.
accentGatedVoice :: Int -> [Maybe [Double]] -> Pattern Note
accentGatedVoice i = accentGated 27.5 0.5 beats . map (>>= pick)
  where
    pick vs = case drop i vs of
      (x : _) -> Just x
      [] -> Nothing

-- | Средний сюрприз каждого такта у слушателя с краткосрочной памятью.
--
-- Долговременная память смену режима поглощает: она не забывает прежний и
-- просто доучивает объединение. Скачок обязан быть виден именно у
-- краткосрочной, потому что у неё меняется локальная статистика.
perBar :: [Bar s Int] -> [Double]
perBar bs = map mean (chunksOf beats ss)
  where
    ss = onlineSurprisalsSeg (newListenerWith (barOrder barOpts) 3 alphabet) (map barSyms bs)
    mean xs = sum xs / fromIntegral (length xs)
