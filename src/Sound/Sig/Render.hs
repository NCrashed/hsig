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
  , Stem (..)
  , stemOf
  , renderStem
  , mixStems
  , mixStemsStereo
  , renderTrack
  , renderTrackWith
  ) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Numeric (showHex)
import Sound.Sig.Core
import Sound.Sig.IO
import Sound.Sig.Random (wordAt)
import Sound.Sig.Score
import Sound.Sig.Stereo
import System.Directory (doesFileExist, listDirectory, removeFile)
import System.FilePath (takeDirectory, takeExtension, takeFileName, (</>))

-- Планировщик ---------------------------------------------------------------

-- | Играет паттерн инструментом. Сигнал бесконечен: длину задаёт тот, кто
-- рендерит.
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
overlapAdd :: Int -> U.Vector Double -> [(Int, U.Vector Double)] -> U.Vector Double
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
  , stemSpec :: String
  -- ^ описание стема: по нему считается ключ кэша, см. 'renderStem'
  , stemPan :: Double
  -- ^ панорама в миксе: -1 слева, 0 по центру, 1 справа
  , stemSig :: Sig
  }

-- | Стем по центру с пустой спецификацией.
stemOf :: String -> String -> Sig -> Stem
stemOf name spec sig =
  Stem {stemName = name, stemSpec = spec, stemPan = 0, stemSig = sig}

-- | Пишет стем в @dir\/<имя>-<хэш>.wav@ и отдаёт путь. Если файл с таким
-- хэшем уже есть, рендер пропускается.
--
-- ВАЖНО: ключ кэша считается от имени, 'stemSpec', длины в сэмплах, частоты
-- и 'envSeed', а не от содержимого сигнала. Сериализовать 'Sig' нечем, это
-- функция (разд. 3), поэтому спецификацию пишет автор трека. Поправили
-- патч, но не поправили спецификацию - получите старый звук молча.
-- Сомневаетесь - удалите файл.
--
-- Спецификация обязана покрывать и то, от чего стем зависит через
-- разделяемые сигналы: если стем качается сайдчейном от бочки, правка бочки
-- меняет и его звук, а хэш об этом не узнает.
--
-- 'envBlock' в ключ намеренно не входит: от размера блока результат не
-- зависит (это отдельно проверено тестами), и включать его значило бы
-- перерендеривать всё при настройке блока.
--
-- Режет по времени жёстко: если к этому моменту нота не успела затухнуть,
-- на конце будет щелчок. Гасить края это дело трека, библиотека не
-- подмешивает фейд молча.
--
-- Стемы пишутся во float32: промежуточный носитель не должен добавлять
-- шума квантования.
renderStem :: Env -> Double -> FilePath -> Stem -> IO FilePath
renderStem env secs dir stem = do
  ready <- doesFileExist path
  if ready
    then pure path
    else do
      _ <- writeWav env Float32 path (takeSec secs (stemSig stem))
      pure path
  where
    path = stemPath env secs dir stem

-- | Путь стема. Имя и спецификация вместе определяют файл, поэтому одно имя
-- с разными спецификациями это разные файлы, а не конфликт.
stemPath :: Env -> Double -> FilePath -> Stem -> FilePath
stemPath env secs dir stem =
  dir </> (stemName stem <> "-" <> stemHash env secs stem <> ".wav")

-- | Компоненты сворачиваются цепочкой, а не складываются: у суммы любая
-- компенсирующая пара правок (seed на единицу вверх, длина на сэмпл вниз)
-- давала бы то же имя файла. Длина берётся в сэмплах, ровно как её понимает
-- takeSec: округление до миллисекунд склеивало бы разные длины в один файл.
stemHash :: Env -> Double -> Stem -> String
stemHash env secs stem = pad (showHex (foldl step 7 parts `mod` 0x100000000) "")
  where
    parts =
      map fromEnum (stemName stem <> "\0" <> stemSpec stem)
        <> [round (secs * envRate env), round (envRate env), envSeed env]
    step acc x = fromIntegral (wordAt (acc + x) 1)
    pad s = replicate (8 - length s) '0' <> s

-- | Складывает стемы из файлов. Читает их целиком, поэтому возвращает
-- сигнал в IO, а не притворяется чистым через unsafePerformIO.
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

readStem :: Env -> FilePath -> IO Sig
readStem env path = do
  (rate, channels, xs) <- readWav path
  when (rate /= envRate env) $
    ioError (userError (path <> ": частота " <> show rate <> " не совпадает с рендером"))
  when (channels /= 1) $
    ioError (userError (path <> ": стем должен быть моно, каналов " <> show channels))
  -- Режем вектор срезами, а не через список: боксированный список стоил бы
  -- десятки байт на сэмпл поверх и без того целиком прочитанного файла.
  -- Блоки это срезы одного массива, но он и так живёт всё время чтения.
  pure (Sig (\e -> blocksOf (blockOf e) xs))
  where
    blocksOf n v
      | U.null v = []
      | otherwise = U.take n v : blocksOf n (U.drop n v)

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
renderTrack :: Env -> Double -> FilePath -> [Stem] -> IO FilePath
renderTrack env secs path = renderTrackWith env secs path id

-- | То же, но сведённый мастер перед записью проходит через обработку:
-- место под насыщение, общий трим или что угодно поперёк каналов.
--
-- Обработка идёт при сведении, а не в стемах, поэтому её правка не сбивает
-- кэш и повторный прогон стоит одно чтение файлов.
--
-- Убирает из каталога устаревшие стемы: при правке спецификации меняется
-- хэш, и без уборки каталог зарастает файлами прошлых версий. Удаляются
-- только файлы вида @<имя>-<хэш>.wav@ для имён из этого трека и только с
-- чужим хэшем; всё остальное в каталоге не трогается. Отсюда правило: два
-- трека с общими именами стемов в одном каталоге будут вычищать кэш друг
-- друга, разводите их по каталогам.
renderTrackWith :: Env -> Double -> FilePath -> (Stereo -> Stereo) -> [Stem] -> IO FilePath
renderTrackWith env secs path master stems = do
  -- Совпали имя и спецификация, а сигналы разные: оба стема писали бы в один
  -- файл одновременно, и в миксе оказался бы один из них дважды. От сигнала
  -- путь зависеть не может, поэтому ловим здесь.
  case duplicates (map (stemPath env secs dir) stems) of
    [] -> pure ()
    dups ->
      ioError . userError $
        "hsig: в один файл пишут несколько стемов: "
          <> unwords [stemName s | s <- stems, stemPath env secs dir s `elem` dups]
  paths <- inParallel (map (renderStem env secs dir) stems)
  sweepStale dir (map stemName stems) paths
  Stereo l r <- master <$> mixStemsStereo env (zip (map stemPan stems) paths)
  _ <- writeWavStereo env Bits16 path (takeSec secs l) (takeSec secs r)
  pure path
  where
    dir = takeDirectory path

-- | Удаляет стемы с устаревшими хэшами. Имя должно быть ровно
-- @<имя>-<восемь шестнадцатеричных>.wav@ для имени из трека, иначе файл не
-- наш и остаётся на месте.
sweepStale :: FilePath -> [String] -> [FilePath] -> IO ()
sweepStale dir names keep = do
  files <- listDirectory dir
  mapM_ (removeFile . (dir </>)) (filter stale files)
  where
    current = map takeFileName keep
    stale f = f `notElem` current && any (`owns` f) names
    owns n f = case splitAt (length n + 1) f of
      (start, rest) ->
        start == n <> "-"
          && length rest == 12
          && takeExtension rest == ".wav"
          && all (`elem` "0123456789abcdef") (take 8 rest)

-- | Значения, встретившиеся больше одного раза, по одному разу каждое.
duplicates :: [String] -> [String]
duplicates = go []
  where
    go _ [] = []
    go seen (x : xs)
      | x `elem` xs && x `notElem` seen = x : go (x : seen) xs
      | otherwise = go seen xs
