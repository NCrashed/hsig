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

-- | Символ это ступень лада, всегда и везде. На сильной доле он объявляет
-- новую гармонию, на остальных - берёт ноту арпеджио.
alphabet :: [Int]
alphabet = [0 .. 6]

-- | Куда разрешается ступень. Это и есть функциональная гармония, записанная
-- как распределение.
--
-- Ради неё всё и затевалось: ступени различимы не тем, как они звучат, а
-- тем, куда они идут. Доминанта это «почти наверняка в тонику», и метрика
-- видит именно это. Если бы разрешение было одинаковым, все ступени были бы
-- бисимилярны и укладка потеряла бы смысл.
resolves :: Int -> [(Int, Double)]
resolves d = case d of
  0 -> [(3, 0.30), (4, 0.30), (5, 0.20), (0, 0.20)] -- i: куда угодно
  1 -> [(4, 0.70), (0, 0.15), (3, 0.15)] -- ii: в доминанту
  2 -> [(5, 0.50), (3, 0.30), (6, 0.20)] -- III: по квинтам вниз
  3 -> [(4, 0.60), (0, 0.20), (1, 0.20)] -- iv: в доминанту
  4 -> [(0, 0.80), (5, 0.15), (4, 0.05)] -- V: в тонику
  5 -> [(1, 0.40), (3, 0.30), (4, 0.30)] -- VI: в предъикт
  _ -> [(2, 0.60), (0, 0.25), (4, 0.15)] -- VII: в медианту

-- | Фигура арпеджио по долям: вверх и вниз по тонам аккорда.
--
-- Почти детерминированная, и это тоже стиль: чиптюновое арпеджио не
-- случайная последовательность, а узнаваемая фигура. Отсюда же берётся
-- профиль сюрприза - доли дёшевы, сильная доля дорога.
figure :: Int -> Int
figure p = [0, 0, 2, 4, 2, 0, 4, 2] !! (p `mod` beats)

arpeggio :: Int -> [(Int, Double)]
arpeggio p = (figure p, 0.85) : rest
  where
    rest = [(x, w) | (x, w) <- spread, x /= figure p]
    spread = [(0, 0.05), (2, 0.05), (4, 0.05), (6, 0.03), (1, 0.01), (3, 0.01), (5, 0.01)]

-- | Состояние: доля такта и ступень. На нулевой доле ступень это та, что
-- закончилась; символ выбирает следующую.
--
-- Машина унифилярна: новая ступень читается прямо из объявляющего символа.
church :: Machine (Int, Int) Int
church =
  Machine
    { machineStart = (0, 0)
    , machineStates = [(p, d) | p <- [0 .. beats - 1], d <- [0 .. 6]]
    , machineOut = \(p, d) -> D.dist (if p == 0 then resolves d else arpeggio p)
    , machineStep = \(p, d) x ->
        if p == 0
          then (1, if x `elem` map fst (resolves d) then x else d)
          else ((p + 1) `mod` beats, d)
    }

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
degreeDist = distMatrixWith 5 defaultGamma [asPred (0, d) | d <- [0 .. 6]]
  where
    asPred s = unfoldPred (machineOut church) (machineStep church) s

degreeChords :: [Chord]
degreeChords = embed opts degreeDist
  where
    opts = defaultEmbed {embedScale = tonality, embedSpan = 5}

-- | Аккорд состояния. На сильной доле держится прежняя гармония: смена
-- приходит на вторую долю, то есть задержанием, как и положено.
chordOf :: (Int, Int) -> Chord
chordOf (_, d) = degreeChords !! d

-- Пьеса ---------------------------------------------------------------------

barOpts :: BarOpts
barOpts = defaultBarOpts {barLen = beats, barCands = 32, barVlMax = 6, barOrder = 3}

bars :: Int -> [Bar (Int, Int) Int]
bars n = compose barOpts church chordOf tonality alphabet (max 1 n)

-- | Ноты: гармония по состояниям, мелодия по символам, пилы второго плана.
linesOf :: [Bar (Int, Int) Int] -> ([[Double]], [Double], [Double], [[Double]])
linesOf bs = (voices, lead, bassLine, sawVoices)
  where
    states = concatMap barStates bs
    syms = concatMap barSyms bs
    voices = voiceLines tonality 35 (map chordOf states)
    -- Символ это ступень лада: мелодия читается напрямую, без переводов.
    lead = degreeLine tonality 64 [root st + s | (st, s) <- zip states syms]
    bassLine = degreeLine tonality 16 [root st | st <- states]
    -- Основание и квинта, регистр между басом и органом. Полудиапазон
    -- узкий: партия второго плана не должна разъезжаться по регистру,
    -- иначе она перестаёт быть подложкой и начинает спорить с органом.
    sawVoices = voiceLinesIn tonality 22 6 [mkChord [root st, root st + 4] | st <- states]
    root (_, d) = minimum (chordDegrees (degreeChords !! d))

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

track :: String -> [Bar (Int, Int) Int] -> [Stem]
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
    harm = harmonyPattern 27.5 beats voices
    lead' = accentPattern 27.5 0.45 beats lead
    bassPat = accentPattern 27.5 0.6 beats bassLine
    -- Голоса пил разнесены по стемам, а не сложены в один: панорама у них
    -- разная, а стем несёт одну панораму.
    sawPatL = accentHarmony 27.5 0.5 beats (map (take 1) sawLines)
    sawPatR = accentHarmony 27.5 0.5 beats (map (drop 1) sawLines)
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
  printf "ошибка модели, каждый четвёртый такт: %s\n" (unwords [printf "%5.2f" (barError b) :: String | b <- everyNth 4 bs])
  printf "ступени по тактам: %s\n" (concatMap (show . snd . head' . barStates) bs)
  printf "сюрприз по доле: %s   граница/остальные %.2f\n" (unwords [printf "%5.2f" v :: String | v <- profileOf bs]) (frontRatio (profileOf bs))
  printf "нот: гармония %d, ведущий %d, бас %d, пилы %d\n" (3 * runs hv) (runs hl) (runs hb) (2 * runs hs)
  if quiet
    then putStrLn "рендер пропущен"
    else renderTrack defaultEnv (path ".wav") (track name bs) >>= putStrLn
  where
    everyNth k xs = [x | (i, x) <- zip [0 :: Int ..] xs, i `mod` k == 0]
    head' xs = case xs of
      (x : _) -> x
      [] -> error "keygen: такт без состояний"
    runs :: (Eq a) => [a] -> Int
    runs evs = sum [length (runsOf bar) | bar <- chunksOf beats evs]

