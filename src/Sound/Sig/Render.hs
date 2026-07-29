-- | Планировщик и стемы.
--
-- Каждое событие паттерна рендерится инструментом в конечный сигнал и
-- складывается overlap-add в поток стема со смещением по своему онсету
-- (разд. 7 дизайна). Стемы пишутся на диск по отдельности, микс собирается
-- из файлов (разд. 8): ленивый список блоков, использованный дважды,
-- удерживался бы целиком.
module Sound.Sig.Render
  ( play
  , Stem (..)
  , renderStem
  , mixStems
  , renderTrack
  ) where

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
import System.FilePath (takeDirectory, (</>))

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
          (out, rest) = U.splitAt block buf
   in go (0 :: Int) U.empty

-- | Событие считается запущенным, если начало его целого отрезка попало в
-- полуинтервал блока.
starts :: Time -> Time -> Event a -> Bool
starts from to e = t >= from && t < to
  where
    t = maybe (arcStart (eventPart e)) arcStart (eventWhole e)

-- | Потолок длины одной ноты. По разд. 7 сигнал инструмента обязан быть
-- конечным; бесконечный вешал бы рендер молча и без объяснения, поэтому
-- лучше упасть с внятной ошибкой.
maxNoteSec :: Double
maxNoteSec = 60

-- | Смещение ноты в сэмплах относительно начала блока и её сигнал.
renderNote :: Env -> Instrument -> Double -> Int -> Event Note -> (Int, U.Vector Double)
renderNote env inst rate blockStart e = (round (onset * rate) - blockStart, samples)
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
    samples
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
  , stemSig :: Sig
  }

-- | Пишет стем в @dir\/<имя>-<хэш>.wav@ и отдаёт путь.
--
-- Режет по времени жёстко: если к этому моменту нота не успела затухнуть,
-- на конце будет щелчок. Гасить края это дело трека, библиотека не
-- подмешивает фейд молча.
--
-- Стемы пишутся во float32: промежуточный носитель не должен добавлять
-- шума квантования. Хэш пока считается от имени, Env и длины; настоящий
-- ключ по содержимому стема придёт вместе с кэшем на M8 (разд. 8).
renderStem :: Env -> Double -> FilePath -> Stem -> IO FilePath
renderStem env secs dir stem = do
  _ <- writeWav env Float32 path (takeSec secs (stemSig stem))
  pure path
  where
    path = dir </> (stemName stem <> "-" <> stemHash env secs stem <> ".wav")

stemHash :: Env -> Double -> Stem -> String
stemHash env secs stem = pad (showHex (wordAt seed 1 `mod` 0x100000000) "")
  where
    seed = sum (map fromEnum (stemName stem)) + round (secs * 1000) + round (envRate env) + envSeed env
    pad s = replicate (8 - length s) '0' <> s

-- | Складывает стемы из файлов. Читает их целиком, поэтому возвращает
-- сигнал в IO, а не притворяется чистым через unsafePerformIO.
mixStems :: Env -> [FilePath] -> IO Sig
mixStems env paths = do
  parts <- mapM readOne paths
  -- Складываем без нейтрального элемента: литеральный 0 бесконечен и
  -- растянул бы микс.
  pure $ case parts of
    [] -> fromSamples []
    p : ps -> foldl (+) p ps
  where
    readOne p = do
      (rate, xs) <- readWav p
      when (rate /= envRate env) $
        ioError (userError (p <> ": частота " <> show rate <> " не совпадает с рендером"))
      pure (fromSamples (U.toList xs))

-- | Рендерит стемы на диск рядом с треком, сводит их и пишет мастер.
renderTrack :: Env -> Double -> FilePath -> [Stem] -> IO FilePath
renderTrack env secs path stems = do
  paths <- mapM (renderStem env secs (takeDirectory path)) stems
  mixed <- mixStems env paths
  _ <- writeWav env Bits16 path (takeSec secs mixed)
  pure path
