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
alphabet = [0 .. 6]

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

-- | Веса трёх полюсов в момент @t@ от нуля до единицы.
--
-- Треугольный путь: начало у первого полюса, середина у второго, конец у
-- третьего. Между ними всё промежуточное, и в этом всё дело - модель не
-- переключается, а **ползёт**.
poles :: Double -> [Double]
poles t = [max 0 (1 - 2 * t), 1 - abs (2 * t - 1), max 0 (2 * t - 1)]

-- | Разрешения в момент @t@: смесь трёх таблиц по весам полюсов.
--
-- Три режима с меткой слушатель выучивал целиком вместе с меткой, и
-- переключение переставало ему что-либо стоить: это были не разные модели, а
-- разные состояния одной. Здесь метки нет, промежуточные распределения не
-- повторяются, и выученное устаревает по ходу.
resolvesAt :: Double -> Int -> [(Int, Double)]
resolvesAt t d =
  [ (x, sum [w * lookupW r x | (r, w) <- zip [0 ..] ws, w > 0])
  | x <- [0 .. 6]
  ]
  where
    ws = poles t
    lookupW r x = maybe 0 id (lookup x (resolvesIn r d))

-- | Острота фигуры арпеджио в момент @t@: от механической к вольной.
--
-- Второй дрейфующий параметр, независимый от гармонического. Фактура
-- расслабляется по ходу пьесы, и это тоже надо переучивать.
sharpnessAt :: Double -> Double
sharpnessAt t = 0.9 - 0.28 * t

-- | Фигура арпеджио: тоже смесь трёх по весам полюсов, и потому она сама
-- ползёт, а не переключается.
figureAt :: Double -> Int -> [(Int, Double)]
figureAt t p =
  [ (x, base x + sharp * sum [w * ind r x | (r, w) <- zip [0 ..] (poles t)])
  | x <- [0 .. 6]
  ]
  where
    sharp = sharpnessAt t
    base x = if x `elem` [0, 2, 4] then 0.04 else 0.01
    ind r x = if table r !! (p `mod` beats) == x then 1 else 0
    table r = case (r :: Int) of
      0 -> [0, 0, 2, 4, 2, 0, 4, 2]
      1 -> [0, 4, 2, 4, 0, 2, 4, 2]
      _ -> [0, 2, 4, 6, 4, 2, 0, 2]

-- | Машина в момент @t@. Состояние прежнее - доля такта и ступень.
--
-- Метки режима в состоянии нет намеренно: она делала бы дрейф выучиваемым
-- целиком. Время живёт снаружи, в индексе семейства.
churchAt :: Double -> Machine (Int, Int) Int
churchAt t =
  Machine
    { machineStart = (0, 0)
    , machineStates = [(p, d) | p <- [0 .. beats - 1], d <- [0 .. 6]]
    , machineOut = \(p, d) -> D.dist (if p == 0 then resolvesAt t d else figureAt t p)
    , machineStep = \(p, d) x ->
        if p == 0 then (1, x) else ((p + 1) `mod` beats, d)
    }

-- | Активность партии в момент @t@: сколько её слышно, от нуля до единицы.
--
-- Партии не включаются рубильником, а **заполняются**: сперва сильные доли,
-- потом дробление. Так и слышно вступление живой партии, и так дрейф
-- остаётся дрейфом на всех слоях, а не только в вероятностях.
activity :: String -> Double -> Double
activity part t = case part of
  "organ" -> clamp (1.15 - t) -- уходит к концу
  "chip" -> clamp (t * 1.8 - 0.05) -- вступает сразу и набирает
  _ -> clamp (t * 2.0 - 0.3) -- пилы подхватывают следом
  where
    clamp = max 0 . min 1

-- | Приоритет доли: чем меньше, тем раньше доля появляется.
priority :: Int -> Double
priority p = [0.0, 0.72, 0.42, 0.88, 0.2, 0.8, 0.55, 0.95] !! (p `mod` beats)

plays :: String -> Double -> Int -> Bool
plays "organ" t _ = activity "organ" t >= 0.35
plays part t p = priority p <= activity part t

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
-- Представители берутся у машины середины пьесы: параметры дрейфуют, а
-- палитра должна быть одна, иначе гармония поплывёт вместе с ними и
-- слушатель потеряет опору.
degreeDist = distMatrixWith 5 defaultGamma [asPred (0, d) | d <- [0 .. 6]]
  where
    mid = churchAt 0.5
    asPred s = unfoldPred (machineOut mid) (machineStep mid) s

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
chordOf :: (Int, Int) -> Chord
chordOf (_, d) = degreeChords !! d

-- Пьеса ---------------------------------------------------------------------

barOpts :: BarOpts
barOpts = defaultBarOpts {barLen = beats, barCands = 32, barVlMax = 6, barOrder = 3}

-- | Время нормируется на длину пьесы: дрейф проходит один и тот же путь
-- независимо от того, сорок восемь тактов или сто.
bars :: Int -> [Bar (Int, Int) Int]
bars n = composeWith barOpts at chordOf tonality alphabet total
  where
    total = max 1 n
    at i = churchAt (timeAt total i)

-- | Нормированное время такта: фора на старте и постоянная скорость.
--
-- Сначала было искривление @u ** 0.6@ - быстро в начале, медленно к концу.
-- Начало оно чинило, но платило за это серединой, и это слышно. Скорость
-- дрейфа у такой степени падает ниже равномерной уже к трети пьесы:
--
-- > u=0.05 -> 1.9    u=0.3 -> 0.98    u=0.5 -> 0.79    u=0.9 -> 0.63
--
-- И ровно там был провал в ошибке модели: такты 9-20 давали самые низкие
-- значения за всю пьесу, то есть слушатель догонял и шёл вровень.
--
-- Причина общая и её стоит запомнить: **путь дрейфа фиксирован, поэтому
-- ускорение в одном месте это замедление в другом**. Перераспределять
-- нечего - надо либо удлинять путь, либо не трогать скорость.
--
-- Здесь выбрано второе: скорость постоянна, а вялое начало чинится форой.
-- Пьеса стартует не с края пути, а с пятой его части, где уже есть смесь
-- полюсов и работают партии.
timeAt :: Int -> Int -> Double
timeAt total i
  | total <= 1 = headStart
  | otherwise = headStart + (1 - headStart) * fromIntegral i / fromIntegral (total - 1)
  where
    headStart = 0.18

-- | Ноты по партиям, с пропусками там, где режим партию не пускает.
--
-- Голоса ведутся насквозь, а гасятся уже после: голосоведение не должно
-- рваться оттого, что партия помолчала - вернувшись, она обязана
-- продолжить с той же высоты, а не начать заново.
linesOf ::
  [Bar (Int, Int) Int] ->
  ([Maybe [Double]], [Maybe Double], [Double], [Maybe [Double]])
linesOf bs = (onlyIn "organ" voices, onlyIn "chip" lead, bassLine, onlyIn "saws" sawVoices)
  where
    states = concatMap barStates bs
    syms = concatMap barSyms bs
    total = length bs
    -- Маска на каждое событие: партия появляется по долям, а не рубильником.
    mask part = [plays part (timeAt total i) p | i <- [0 .. total - 1], p <- [0 .. beats - 1]]
    onlyIn part xs = [if on then Just x else Nothing | (on, x) <- zip (mask part) xs]

    -- Звучащий аккорд считается по состоянию **и символу** вместе.
    --
    -- На сильной доле состояние ещё несёт прежнюю ступень, а новую называет
    -- символ, излучаемый ровно на этом событии. Брать только состояние
    -- значило откладывать смену гармонии на вторую долю - и это слышалось
    -- не задержанием, а опозданием: задержание держит диссонанс на сильной
    -- доле и разрешает на слабой, а тут на сильной стоял прежний аккорд, и
    -- новый вваливался мимо неё.
    chordsAt = [sounding st s | (st, s) <- zip states syms]
    sounding (0, _) s = degreeChords !! s
    sounding (_, d) _ = degreeChords !! d
    voices = voiceLines tonality 35 chordsAt
    -- Символ это ступень лада: мелодия читается напрямую, без переводов.
    -- Символы 7 и 8 объявляют смену режима и звучат скачком вверх - это
    -- ровно то место, где происходит поворот, и слышать его надо.
    -- Символ значит разное на разных долях, и мелодия обязана это учитывать.
    -- На долях арпеджио он смещение по тону аккорда, и root + s даёт
    -- аккордовый тон. На сильной доле он имя следующей ступени, и то же
    -- сложение давало произвольный тон лада поверх ещё звучащего прежнего
    -- аккорда - неаккордовый на большинстве сильных долей, отсюда и
    -- диссонанс на каждом стыке.
    --
    -- Теперь объявление звучит основанием объявленной ступени: это
    -- предъём, гармония приходит следом, а сведений в ноте столько же -
    -- ступень по ней читается по-прежнему.
    lead = degreeLine tonality 64 [leadDeg st s | (st, s) <- zip states syms]
    leadDeg (0, _) s = rootOf s
    leadDeg (_, d) s = rootOf d + s
    rootOf d = minimum (chordDegrees (degreeChords !! d))
    -- Бас и пилы идут по тому же звучащему аккорду, иначе низ разъедется с
    -- гармонией ровно на сильной доле.
    roots = [minimum (chordDegrees c) | c <- chordsAt]
    bassLine = degreeLine tonality 16 roots
    -- Основание и квинта, регистр между басом и органом. Полудиапазон
    -- узкий: партия второго плана не должна разъезжаться по регистру,
    -- иначе она перестаёт быть подложкой и начинает спорить с органом.
    sawVoices = voiceLinesIn tonality 22 6 [mkChord [r, r + 4] | r <- roots]

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
  printf "состояний %d; h_mu и C_mu по ходу дрейфа:\n" (length (machineStates (churchAt 0)))
  printf "  t=0.0  h_mu %.3f  C_mu %.3f\n" (entropyRate (churchAt 0)) (statComplexity (churchAt 0))
  printf "  t=0.5  h_mu %.3f  C_mu %.3f\n" (entropyRate (churchAt 0.5)) (statComplexity (churchAt 0.5))
  printf "  t=1.0  h_mu %.3f  C_mu %.3f\n" (entropyRate (churchAt 1)) (statComplexity (churchAt 1))
  printf "липшиц = %.3f, искажение = %.3f\n" (lipschitz tonality degreeChords degreeDist) (distortion tonality degreeChords degreeDist)
  printf "ступени в полутонах: %s\n" (show [map (`mod` 12) (chordSemis tonality c) | c <- degreeChords])
  writeFile (path "-kernel.svg") (ringSvg defaultTheme (churchAt 0.5) (const "") 0.06)
  writeFile (path "-trace.svg") (traceSvg defaultTheme (churchAt 0.5) beats 16 states)
  printf "картинки: %s-kernel.svg, %s-trace.svg\n" (path "") (path "")
  printf "ошибка по тактам: %s\n" (unwords [printf "%.2f" (barError b) :: String | b <- bs])
  printf "ступени по тактам: %s\n" (concatMap (show . deg . head' . barStates) bs)
  printf "сюрприз по доле: %s   граница/остальные %.2f\n" (unwords [printf "%5.2f" v :: String | v <- profileOf bs]) (frontRatio (profileOf bs))
  printf "средний сюрприз, бит:\n"
  printf "  только долгая        %.3f\n" (whole 0 1 bs)
  printf "  краткая, сброс 1 такт %.3f\n" (whole 3 1 bs)
  printf "  краткая, сброс 4 такта %.3f\n" (whole 3 4 bs)
  printf "  краткая, сброс 8 тактов %.3f\n" (whole 3 8 bs)
  printf "  краткая, сброс 16 тактов %.3f\n" (whole 3 16 bs)
  printf "нот: гармония %d, ведущий %d, бас %d, пилы %d\n" (3 * runs hv) (runs hl) (runs hb) (2 * runs hs)
  if quiet
    then putStrLn "рендер пропущен"
    else renderTrack defaultEnv (path ".wav") (track name bs) >>= putStrLn
  where
    deg (_, d) = d
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

-- | Средний сюрприз пьесы у слушателя с краткосрочной памятью заданного
-- порядка. Ноль означает «только долговременная».
--
-- Это решающий замер для дрейфа. Долговременная память не забывает и
-- потому тащит за собой устаревшую статистику; краткосрочная помнит только
-- последнюю фразу и обязана оказаться ближе к тому, что происходит сейчас.
-- На стационарных машинах она не давала ничего - если не даст и здесь,
-- значит дрейф не работает.
whole :: Int -> Int -> [Bar s Int] -> Double
whole short period bs = mean (onlineSurprisalsSeg lis segs)
  where
    lis = newListenerWith (barOrder barOpts) short alphabet
    segs = map concat (chunksOf (max 1 period) (map barSyms bs))
    mean xs = sum xs / fromIntegral (length xs)
