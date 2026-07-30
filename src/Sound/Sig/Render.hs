-- | Планировщик и стемы.
--
-- Каждое событие паттерна рендерится инструментом в конечный сигнал и
-- складывается overlap-add в поток стема со смещением по своему онсету
-- (разд. 7 дизайна). Стемы пишутся на диск по отдельности, микс собирается
-- из файлов (разд. 8): ленивый список блоков, использованный дважды,
-- удерживался бы целиком.
module Sound.Sig.Render
  ( play
  , playStereo
  , Stem
  , stem
  , cached
  , panned
  , stereoStems
  , renderStem
  , mixStems
  , renderTrack
  , renderTrackWith
  ) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, SomeException, catch, throwIO, try)
import Control.Monad (forM_, unless, when)
import Control.Monad.ST (runST)
import Data.List (sortOn, stripPrefix)
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (Down (..))
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Numeric (showHex)
import Sound.Sig.Core
import Sound.Sig.IO
import Sound.Sig.Random (wordAt)
import Sound.Sig.Score
import Sound.Sig.Stereo
import System.Directory (doesFileExist, getModificationTime, listDirectory, removeFile, renameFile)
import System.FilePath (takeDirectory, takeExtension, takeFileName, (</>))
import System.IO (hPutStrLn, stderr)

-- Планировщик ---------------------------------------------------------------

-- | Играет паттерн инструментом. Сигнал бесконечен, потому что паттерн
-- бесконечен: длину задаёт материал трека (умножение на окно или огибающую),
-- рендер её не подрезает.
--
-- Нота запускается, когда начало её целого отрезка попадает в текущий блок,
-- поэтому каждое событие срабатывает ровно один раз.
play :: Instrument -> Pattern Note -> Sig
play inst pat = Sig $ \env ->
  let block = blockOf env
      rate = envRate env
      go !i tailBuf = out : go (i + 1) rest
        where
          from = fromIntegral (i * block) / toRational rate
          to = fromIntegral ((i + 1) * block) / toRational rate
          fired = filter (starts from to) (queryArc pat (Arc from to))
          parts = map (renderNote env inst rate (i * block)) fired
          buf = overlapAdd block tailBuf parts
          -- force обязателен: splitAt отдаёт срезы одного массива, и блок в
          -- 4096 сэмплов держал бы буфер на всю длину самой долгой ноты. Для
          -- потокового потребителя это незаметно, а share или render на таком
          -- сигнале раздували бы память в needed/block раз.
          out = U.force (U.take block buf)
          rest = U.drop block buf
   in go (0 :: Int) U.empty

-- | Играет паттерн стерео-инструментом.
--
-- Цена: нота рендерится дважды, по разу на канал. Sig отдаёт один поток
-- (разд. 3), поэтому один проход дал бы только моно.
playStereo :: (Note -> Stereo) -> Pattern Note -> Stereo
playStereo inst pat =
  Stereo (play (leftChan . inst) pat) (play (rightChan . inst) pat)

-- | Событие считается запущенным, если начало его целого отрезка попало в
-- полуинтервал блока и этот фрагмент несёт атаку.
--
-- Проверка атаки (в Tidal это eventHasOnset) обязательна: cat и всё, что
-- построено на splitQueries, режет запрос по границам циклов и отдаёт одну
-- ноту двумя фрагментами с одним и тем же целым отрезком. Без неё нота,
-- начавшаяся перед границей цикла и тянущаяся за неё, запускалась бы дважды
-- и звучала вдвое громче, если начало и граница попали в один блок.
starts :: Time -> Time -> Event a -> Bool
starts from to e = case eventWhole e of
  -- Как eventHasOnset в Tidal: у аналогового события целого отрезка нет,
  -- значит нет ни атаки, ни длительности. Запускать его нельзя: длину ноты
  -- пришлось бы брать из части, то есть из размера блока, и число нот с их
  -- длиной начало бы зависеть от envBlock.
  Nothing -> False
  Just w -> t == arcStart (eventPart e) && t >= from && t < to
    where
      t = arcStart w

-- | Потолок длины одной ноты. По разд. 7 сигнал инструмента обязан быть
-- конечным; бесконечный вешал бы рендер молча и без объяснения, поэтому
-- лучше упасть с внятной ошибкой.
maxNoteSec :: Double
maxNoteSec = 60

-- | Смещение ноты в сэмплах относительно начала блока и её сигнал.
renderNote :: Env -> Instrument -> Double -> Int -> Event Note -> (Int, U.Vector Double)
renderNote env inst rate blockStart e = (round (onset * rate) - blockStart, checked)
  where
    Arc ws we = fromMaybe (eventPart e) (eventWhole e)
    onset = fromRational ws
    note =
      (eventValue e)
        { noteOnset = onset
        , noteDur = fromRational (we - ws)
        }
    limit = round (maxNoteSec * rate) :: Int
    -- Берём на сэмпл больше предела: иначе takeSec сам обрежет ровно до
    -- него и честная нота длиной ровно в предел была бы неотличима от
    -- бесконечной.
    rendered = render env (takeSec (maxNoteSec + 1 / rate) (inst note))
    checked
      | U.length rendered > limit =
          error
            ( "hsig: инструмент вернул сигнал длиннее "
                <> show maxNoteSec
                <> " с для ноты на "
                <> show (noteDur note)
                <> " с; по разд. 7 он обязан быть конечным (не забыта ли огибающая?)"
            )
      | otherwise = rendered

-- | Складывает хвост от прошлых блоков и новые ноты в один буфер.
--
-- Когда новых нот нет, хвост отдаётся как есть, без копии. Иначе долгая нота
-- переписывалась бы заново на каждом блоке, то есть цена росла бы квадратично
-- по её длине: десятисекундный пэд при 48 кГц это 117 блоков по 480 тысяч
-- сэмплов.
overlapAdd :: Int -> U.Vector Double -> [(Int, U.Vector Double)] -> U.Vector Double
overlapAdd minLen tailBuf parts
  | null parts && U.length tailBuf >= minLen = tailBuf
overlapAdd minLen tailBuf parts = runST $ do
  let needed = maximum (minLen : U.length tailBuf : [o + U.length v | (o, v) <- parts])
  buf <- UM.replicate needed 0
  forM_ [0 .. U.length tailBuf - 1] $ \k ->
    UM.unsafeModify buf (+ U.unsafeIndex tailBuf k) k
  forM_ parts $ \(o, v) ->
    forM_ [0 .. U.length v - 1] $ \k ->
      when (o + k >= 0) $ UM.unsafeModify buf (+ U.unsafeIndex v k) (o + k)
  U.unsafeFreeze buf

-- Стемы ---------------------------------------------------------------------

data Stem = Stem
  { stemName :: String
  , stemSig :: Sig
  , stemPan :: Double
  -- ^ панорама в миксе: -1 слева, 0 по центру, 1 справа
  , stemCache :: Maybe String
  -- ^ описание стема, если кэш включён: по нему считается ключ, см. 'cached'
  }

-- | Стем по имени и сигналу: по центру, без кэша.
--
-- Сигнал обязан быть конечным: длина трека берётся из материала, поэтому
-- заканчивает стем автор - окном ('gate') или огибающей. Рендер ничего не
-- подрезает и края не гасит: если материал обрывается на незатухшей ноте,
-- в файле будет щелчок. Забытое окно ловится потолком в 600 секунд, но
-- ловится дорого, потому что до предела стем честно считается.
--
-- Без кэша значит, что стем считается на каждом прогоне. Это и есть
-- безопасное поведение по умолчанию: забыть обновить спецификацию
-- невозможно, потому что её нет. Кэш включают явно ('cached') там, где
-- рендер долгий.
stem :: String -> Sig -> Stem
stem name sig = Stem {stemName = name, stemSig = sig, stemPan = 0, stemCache = Nothing}

-- | Включает кэш: пока строка не изменилась, файл переиспользуется.
--
-- ВАЖНО: строку пишет автор трека, и она обязана покрывать всё, что меняет
-- звук стема, включая то, от чего он зависит через разделяемые сигналы
-- (например сайдчейн от бочки). Сериализовать 'Sig' нечем, это функция
-- (разд. 3). Поправили патч, но не поправили строку - получите старый звук
-- молча. Сомневаетесь - снимите 'cached', тогда стем всегда свежий.
--
-- Длина в ключ не входит: её задаёт материал, и узнать её заранее нельзя, не
-- отрендерив стем. Поэтому длину тоже описывает строка. В треке она обычно
-- одна на всех, отсюда идиома @cached ("лид v1, " <> show trackSec <> " с")@:
-- сократили трек для прослушивания - ключи поехали сами.
cached :: String -> Stem -> Stem
cached spec s = s {stemCache = Just spec}

-- | Панорама в миксе: -1 слева, 0 по центру, 1 справа, равная мощность.
--
-- Выход за диапазон это ошибка автора (@panned 35@ вместо @panned 0.35@), а
-- не край образа: молчаливый зажим прятал бы опечатку.
panned :: Double -> Stem -> Stem
panned p s
  | isNaN p || abs p > 1 = error ("hsig: панорама вне -1..1: " <> show p)
  | otherwise = s {stemPan = p}

-- | Стерео-стем как пара моно-стемов по краям образа.
--
-- Каналы уже посчитаны (например 'orbit' или широким унисоном), поэтому
-- панорама у них крайняя: при равной мощности -1 и 1 дают ровно (1, 0) и
-- (0, 1), то есть ухо попадает в свой канал без ослабления. Считаются они
-- параллельно, как и любые два стема.
stereoStems :: String -> Stereo -> [Stem]
stereoStems name (Stereo l r) =
  [ panned (-1) (stem (name <> "L") l)
  , panned 1 (stem (name <> "R") r)
  ]

-- | Пишет стем в каталог @dir@ и отдаёт путь к файлу. Без кэша это
-- @dir\/<имя>.wav@ и рендер каждый прогон, с 'cached' - @dir\/<имя>-<хэш>.wav@,
-- и готовый файл переиспользуется.
--
-- Ключ кэша считается от имени стема, спецификации, 'envRate' и 'envSeed'.
-- Что обязана покрывать спецификация - см. 'cached'. 'envBlock' в ключ
-- намеренно не входит: от размера блока результат не зависит (проверено
-- тестами), иначе его настройка перерендеривала бы всё.
--
-- Стемы пишутся во float32: промежуточный носитель не должен добавлять
-- шума квантования.
renderStem :: Env -> FilePath -> Stem -> IO FilePath
renderStem env dir s = do
  ready <- maybe (pure False) (const (doesFileExist path)) (stemCache s)
  if ready
    then pure path
    else do
      -- Пишем под временным именем и переименовываем только после проверки
      -- длины: иначе отвергнутый стем успевал побывать на конечном пути, и
      -- неудачное удаление (файл открыт плеером) оставляло бы его готовым
      -- для кэша следующего прогона.
      writeChecked env building (stemSig s) (tooLong env ("стем " <> stemName s))
      renameFile building path
      pure path
  where
    path = stemPath env dir s
    building = path <> ".part"

-- | Пишет сигнал во float32 с потолком длины: длиннее 'envMaxSec' считается
-- забытым окном. Файл после отказа не остаётся.
writeChecked :: Env -> FilePath -> Sig -> String -> IO ()
writeChecked env path sig complaint = do
  -- Берём на сэмпл больше предела: иначе takeSec обрезал бы ровно по нему и
  -- честный стем ровно в предел был бы неотличим от бесконечного.
  report <- writeWav env Float32 path (takeSec (envMaxSec env + 1 / envRate env) sig)
  when (clipFrames report > limit) $ do
    removeQuietly path
    ioError (userError complaint)
  where
    limit = round (envMaxSec env * envRate env)

tooLong :: Env -> String -> String
tooLong env what =
  "hsig: "
    <> what
    <> " длиннее "
    <> show (envMaxSec env)
    <> " с, то есть похож на бесконечный. Длина трека берётся из материала,"
    <> " поэтому кончаться он обязан сам: ограничьте его окном (gate) или"
    <> " огибающей. Трек честно длиннее - поднимите envMaxSec."

-- | Путь стема. Без кэша это @<имя>.wav@, и файл перезаписывается каждый
-- прогон. С кэшем в имя входит хэш спецификации, поэтому одно имя с разными
-- спецификациями это разные файлы, а не конфликт.
stemPath :: Env -> FilePath -> Stem -> FilePath
stemPath env dir s =
  dir </> case stemCache s of
    Nothing -> stemName s <> ".wav"
    Just spec -> stemName s <> "-" <> stemHash env spec s <> ".wav"

-- | Компоненты сворачиваются цепочкой, а не складываются: у суммы любая
-- компенсирующая пара правок (seed на единицу вверх, частота на герц вниз)
-- давала бы то же имя файла.
stemHash :: Env -> String -> Stem -> String
stemHash env spec s = pad (showHex (foldl step 7 parts `mod` 0x100000000) "")
  where
    parts =
      map fromEnum (stemName s <> "\0" <> spec)
        <> [round (envRate env), envSeed env]
    step acc x = fromIntegral (wordAt (acc + x) 1)
    pad t = replicate (8 - length t) '0' <> t

-- | Складывает стемы из файлов. Открывает их, поэтому отдаёт сигнал в IO, а
-- не притворяется чистым через unsafePerformIO; блоки тянутся лениво (см.
-- readStem).
mixStems :: Env -> [FilePath] -> IO Sig
mixStems env paths = do
  parts <- mapM (readStem env) paths
  -- Складываем без нейтрального элемента: литеральный 0 бесконечен и
  -- растянул бы микс.
  pure $ case parts of
    [] -> fromSamples []
    p : ps -> foldl (+) p ps

-- | То же с панорамой: каждый стем ставится в своё место образа.
mixStemsStereo :: Env -> [(Double, FilePath)] -> IO Stereo
mixStemsStereo env parts =
  mixStereo <$> mapM (\(p, path) -> pan p <$> readStem env path) parts

-- | Читает стем потоком: блоки тянутся по мере надобности, поэтому память
-- не зависит от длины трека. Ради этого стемы и заведены (разд. 8): пять
-- минут при 384 кГц на пять стемов это гигабайты, если читать целиком.
readStem :: Env -> FilePath -> IO Sig
readStem env path = do
  (rate, channels, blocks) <- readWavBlocks (blockOf env) path
  when (rate /= envRate env) $
    ioError (userError (path <> ": частота " <> show rate <> " не совпадает с рендером"))
  when (channels /= 1) $
    ioError (userError (path <> ": стем должен быть моно, каналов " <> show channels))
  -- Блоки читаются под размер блока этого Env; если потребитель попросит
  -- другой, rechunk переложит, не форсируя лишнего.
  pure (Sig (\e -> rechunk (blockOf e) blocks))

-- | Выполняет действия параллельно, сохраняя порядок результатов.
--
-- Стемы независимы, поэтому рендерятся одновременно. Внутри стема ноты
-- считаются последовательно, и это осознанно: параллелить их стоило бы
-- только на плотных паттернах (от десятка событий в секунду блок начинает
-- ловить по две-три ноты), а плотное это обычно перкуссия, которая и так
-- считается быстро. Дорогие стемы наоборот разреженные: на блок приходится
-- не больше одной ноты, и параллелить там нечего.
inParallel :: forall a. [IO a] -> IO [a]
inParallel actions = do
  boxes <- mapM start actions
  results <- mapM takeMVar boxes
  either throwIO pure (sequence results)
  where
    start act = do
      box <- newEmptyMVar
      _ <- forkIO (try act >>= putMVar box)
      pure (box :: MVar (Either SomeException a))

-- | Рендерит стемы на диск рядом с треком, сводит их по панораме и пишет
-- стерео-мастер в 16 бит.
--
-- Длина берётся из материала: каждый стем считается до своего конца, мастер
-- выходит длиной с самый длинный. Отдельного аргумента длины нет намеренно -
-- он дублировал бы окно, которым стем и так ограничен.
renderTrack :: Env -> FilePath -> [Stem] -> IO FilePath
renderTrack env path = renderTrackWith env path id

-- | То же, но сведённый мастер перед записью проходит через обработку:
-- место под насыщение, общий трим или что угодно поперёк каналов.
--
-- Обработка идёт при сведении, а не в стемах, поэтому её правка не сбивает
-- кэш и повторный прогон стоит одно чтение файлов.
--
-- Убирает из каталога устаревшие стемы: при правке спецификации меняется
-- хэш, и без уборки каталог зарастает файлами прошлых версий. Удаляются
-- только файлы вида @<имя>-<хэш>.wav@ для имён из этого трека, и только те,
-- что старше нескольких последних версий (см. 'keepVersions'); всё остальное
-- в каталоге не трогается. Отсюда два правила: держите треки с общими
-- именами стемов в разных каталогах (иначе вычистят кэш друг друга) и не
-- запускайте два рендера в один каталог одновременно - стемы без кэша
-- называются по имени, и оба прогона пишут в один файл.
renderTrackWith :: Env -> FilePath -> (Stereo -> Stereo) -> [Stem] -> IO FilePath
renderTrackWith env path master stems = do
  -- Совпали имя и спецификация, а сигналы разные: оба стема писали бы в один
  -- файл одновременно, и в миксе оказался бы один из них дважды. От сигнала
  -- путь зависеть не может, поэтому ловим здесь. Путь мастера тоже в списке:
  -- стем с таким же путём затирался бы мастером каждый прогон.
  case duplicates (path : map (stemPath env dir) stems) of
    [] -> pure ()
    dups ->
      ioError . userError $
        "hsig: в один файл пишут несколько стемов: "
          <> unwords ([takeFileName path | path `elem` dups] <> [stemName s | s <- stems, stemPath env dir s `elem` dups])
  paths <- inParallel (map (renderStem env dir) stems)
  sweepStale dir [stemName s | s <- stems, isJust (stemCache s)] paths
  Stereo l r <- master <$> mixStemsStereo env (zip (map stemPan stems) paths)
  -- Сумма по PadZero уже длиной с самый длинный стем, но обработка мастера
  -- может её продлить (реверб-хвост) или сделать бесконечной (+ constant),
  -- поэтому тот же потолок, что у стема. Пишем под временным именем: иначе
  -- отвергнутый мастер побывал бы на конечном пути.
  report <- writeWavStereo env Bits16 building (takeSec cap l) (takeSec cap r)
  when (clipFrames report > limit) $ do
    removeQuietly building
    ioError (userError (tooLong env "мастер"))
  -- Пустой мастер это почти всегда забытое окно или нулевая длина трека:
  -- молча отдать валидный файл на ноль сэмплов хуже, чем сказать.
  when (clipFrames report == 0 && not (null stems)) $
    hPutStrLn stderr ("hsig: " <> path <> ": мастер пустой, ноль кадров")
  renameFile building path
  pure path
  where
    dir = takeDirectory path
    building = path <> ".part"
    cap = envMaxSec env + 1 / envRate env
    limit = round (envMaxSec env * envRate env)

-- | Сколько версий каждого кэшируемого стема остаётся в каталоге.
--
-- Не одна: рабочий цикл это метание между вариантами (превью на восемь
-- секунд и полный прогон, две версии патча), а спецификация у них разная,
-- значит разный и хэш. Уборка "всё, кроме текущего" заставляла бы считать
-- дорогой стем заново при каждом переключении, то есть отменяла бы кэш ровно
-- там, где он нужен. Четыре это две пары вариантов.
keepVersions :: Int
keepVersions = 4

-- | Удаляет старые версии кэш-файлов и докладывает об этом в stderr:
-- разрушительная операция не должна быть молчаливой.
--
-- Имя обязано совпасть с @<имя>-<хэш>.wav@ для имени из этого трека, иначе
-- файл не наш и остаётся на месте. Оговорка: чужой файл, случайно попавший в
-- этот шаблон (@mix-20260730.wav@ рядом со стемом @mix@), от уборки не
-- отличим - держите экспорты не в каталоге стемов. Стемы без кэша сюда не
-- попадают: они пишутся под своим именем и перезаписываются сами.
--
-- Отказы уборки не валят рендер: удалять чужое или несуществующее это не
-- причина терять уже посчитанные стемы. Та же политика, что у dropQuietly
-- при записи. Файл, который не удалось опросить по времени, не удаляется.
sweepStale :: FilePath -> [String] -> [FilePath] -> IO ()
sweepStale dir names keep
  | null names = pure ()
  | otherwise = do
      files <- listDirectory dir `catch` emptyOnError
      stale <- concat <$> mapM (staleFor files) names
      mapM_ (removeQuietly . (dir </>)) stale
      unless (null stale) $
        hPutStrLn stderr ("hsig: убрано устаревших стемов " <> show (length stale) <> ": " <> unwords stale)
  where
    emptyOnError :: IOException -> IO [FilePath]
    emptyOnError _ = pure []
    current = map takeFileName keep
    -- Свежий стем этого прогона уже занимает одно место из keepVersions.
    staleFor files name = do
      dated <- mapM withTime (filter mine files)
      pure (map fst (drop (keepVersions - 1) (sortOn (Down . snd) [(f, t) | (f, Just t) <- dated])))
      where
        mine f = f `notElem` current && hashed (name <> "-") f
    -- Файл, который не удалось опросить, к удалению не предлагается.
    withTime f = do
      r <- try (getModificationTime (dir </> f))
      pure (f, either ignoreTime Just r)
    ignoreTime :: IOException -> Maybe a
    ignoreTime _ = Nothing
    hashed pre f = case stripPrefix pre f of
      Just rest ->
        length rest == 12
          && takeExtension rest == ".wav"
          && all (`elem` "0123456789abcdef") (take 8 rest)
      Nothing -> False

-- | Удаляет файл, не поднимая шума: отказ уборки не причина терять уже
-- посчитанные стемы.
removeQuietly :: FilePath -> IO ()
removeQuietly p = removeFile p `catch` ignore
  where
    ignore :: IOException -> IO ()
    ignore _ = pure ()

-- | Значения, встретившиеся больше одного раза, по одному разу каждое.
duplicates :: [String] -> [String]
duplicates = go []
  where
    go _ [] = []
    go seen (x : xs)
      | x `elem` xs && x `notElem` seen = x : go (x : seen) xs
      | otherwise = go seen xs
