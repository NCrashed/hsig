{-# LANGUAGE OverloadedStrings #-}

-- | Запись WAV: 32-битный float без изменений либо 24/16 бит с TPDF-дизером.
--
-- Клиппинг не подавляется молча: выход за [-1, 1] считается, датируется и
-- уходит в stderr.
module Sound.Sig.IO
  ( BitDepth (..)
  , ClipReport (..)
  , writeWav
  ) where

import Control.Monad (when)
import Data.Bits (shiftR, (.&.))
import Data.ByteString.Builder
import Data.Vector.Unboxed qualified as U
import GHC.Float (double2Float)
import Sound.Sig.Core
import Sound.Sig.Random (doubleAt)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO

data BitDepth
  = Bits16
  | Bits24
  | Float32
  deriving (Eq, Show)

data ClipReport = ClipReport
  { clipCount :: !Int
  , clipFirst :: !(Maybe Double)
  -- ^ время первого выхода за диапазон, секунды
  , clipPeak :: !Double
  -- ^ максимум модуля сигнала
  }
  deriving (Eq, Show)

-- | Пишет моно-WAV, создавая недостающие каталоги.
writeWav :: Env -> BitDepth -> FilePath -> Sig -> IO ClipReport
writeWav env depth path sig = do
  when (rate < 1) $ ioError (userError (badRate env))
  createDirectoryIfMissing True (takeDirectory path)
  stats <- withBinaryFile path WriteMode $ \h -> do
    -- Длина заранее неизвестна, поэтому заголовок дописывается по факту.
    hPutBuilder h (headerFor depth rate 0)
    (frames, stats) <- writeAll h 0 emptyStats (runSig sig env)
    when (odd (frames * bytesPer depth)) $ hPutBuilder h (word8 0)
    hSeek h AbsoluteSeek 0
    hPutBuilder h (headerFor depth rate frames)
    pure stats
  let report = toReport env stats
  when (clipCount report > 0) $ hPutStrLn stderr (clipMessage path report)
  pure report
  where
    rate = round (envRate env)
    bytes = bytesPer depth
    writeAll h !n !st cs = case cs of
      [] -> pure (n, st)
      c : rest
        | (n + U.length c) * bytes > maxDataBytes -> ioError (userError (tooBig path))
        | otherwise -> do
            let (b, st') = encodeChunk env depth n st c
            hPutBuilder h b
            writeAll h (n + U.length c) st' rest

-- | Размеры в WAV 32-битные. Упереться в предел можно только сигналом без
-- ограничения по длине, и тогда честнее упасть, чем дописать заголовок с
-- обрезанным по модулю размером.
maxDataBytes :: Int
maxDataBytes = 0xFFFFFFFF - 64

badRate :: Env -> String
badRate env = "hsig: envRate должен быть положительным, задано " <> show (envRate env)

tooBig :: FilePath -> String
tooBig path =
  path
    <> ": данные не помещаются в 32-битные поля WAV (предел 4 ГиБ), файл оборван."
    <> " Ограничьте сигнал по длине (takeSec) или пишите стемами."

-- Заголовок -------------------------------------------------------------

bytesPer :: BitDepth -> Int
bytesPer Bits16 = 2
bytesPer Bits24 = 3
bytesPer Float32 = 4

isFloat :: BitDepth -> Bool
isFloat Float32 = True
isFloat _ = False

-- | RIFF/WAVE фиксированной длины: 44 байта для PCM, 58 для float (у него
-- поле cbSize и обязательный chunk fact).
headerFor :: BitDepth -> Int -> Int -> Builder
headerFor depth rate frames =
  byteString "RIFF"
    <> word32LE (fromIntegral riffSize)
    <> byteString "WAVE"
    <> byteString "fmt "
    <> word32LE (fromIntegral fmtSize)
    <> word16LE (if isFloat depth then 3 else 1)
    <> word16LE 1
    <> word32LE (fromIntegral rate)
    <> word32LE (fromIntegral (rate * bytes))
    <> word16LE (fromIntegral bytes)
    <> word16LE (fromIntegral (8 * bytes))
    <> (if isFloat depth then word16LE 0 else mempty)
    <> ( if isFloat depth
           then byteString "fact" <> word32LE 4 <> word32LE (fromIntegral frames)
           else mempty
       )
    <> byteString "data"
    <> word32LE (fromIntegral dataSize)
  where
    bytes = bytesPer depth
    dataSize = frames * bytes
    fmtSize = if isFloat depth then 18 else 16 :: Int
    factSize = if isFloat depth then 12 else 0 :: Int
    -- Нечётный chunk дополняется байтом, он входит в размер RIFF.
    riffSize = 4 + (8 + fmtSize) + factSize + (8 + dataSize) + (dataSize `mod` 2)

-- Сэмплы ----------------------------------------------------------------

-- | Накопитель отчёта в сэмплах: время считается уже на выходе.
data Stats = Stats
  { stCount :: !Int
  , stFirst :: !(Maybe Int)
  , stPeak :: !Double
  }

emptyStats :: Stats
emptyStats = Stats {stCount = 0, stFirst = Nothing, stPeak = 0}

toReport :: Env -> Stats -> ClipReport
toReport env st =
  ClipReport
    { clipCount = stCount st
    , clipFirst = (\i -> fromIntegral i / envRate env) <$> stFirst st
    , clipPeak = stPeak st
    }

clipMessage :: FilePath -> ClipReport -> String
clipMessage path r =
  path
    <> ": клиппинг, сэмплов "
    <> show (clipCount r)
    <> ", пик "
    <> show (clipPeak r)
    <> ", первый на "
    <> maybe "?" show (clipFirst r)
    <> " с"

encodeChunk :: Env -> BitDepth -> Int -> Stats -> U.Vector Double -> (Builder, Stats)
encodeChunk env depth i0 st0 = U.ifoldl' step (mempty, st0)
  where
    step (!b, !st) k x =
      let i = i0 + k
       in (b <> encodeSample depth (dither env depth i) x, note i x st)

note :: Int -> Double -> Stats -> Stats
note i x st =
  Stats
    { stCount = if over then stCount st + 1 else stCount st
    , stFirst = case stFirst st of
        Nothing | over -> Just i
        seen -> seen
    , stPeak = max a (stPeak st)
    }
  where
    a = abs x
    over = a > 1

encodeSample :: BitDepth -> Double -> Double -> Builder
encodeSample Float32 _ x = floatLE (double2Float x)
encodeSample Bits16 d x = int16LE (fromIntegral (quantize 16 d x))
encodeSample Bits24 d x = int24LE (quantize 24 d x)

-- | Дизер и noise берут значения из одного генератора, поэтому индексы
-- разведены: без сдвига при @noise 0@ выходило бы точное тождество
-- @dither i == (noise0 (2*i) + noise0 (2*i+1)) \/ 2@. Номер сэмпла до этого
-- сдвига не дотягивается: 2^48 сэмплов это почти двести лет при 48 кГц.
ditherStream :: Int
ditherStream = 0x1000000000000

-- | TPDF из двух независимых равномерных, размах +-1 LSB.
dither :: Env -> BitDepth -> Int -> Double
dither _ Float32 _ = 0
dither env _ i = doubleAt seed (j + 1) + doubleAt seed j - 1
  where
    seed = envSeed env
    j = ditherStream + 2 * i

quantize :: Int -> Double -> Double -> Int
quantize bits d x = max lo (min hi (round (x * scale + d)))
  where
    scale = 2 ^ (bits - 1) :: Double
    hi = 2 ^ (bits - 1) - 1
    lo = negate (2 ^ (bits - 1))

int24LE :: Int -> Builder
int24LE v = word8 (octet 0) <> word8 (octet 1) <> word8 (octet 2)
  where
    octet k = fromIntegral ((v `shiftR` (8 * k)) .&. 0xff)
