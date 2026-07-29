{-# LANGUAGE OverloadedStrings #-}

-- | Запись WAV: заголовок, квантование, дизер, клиппинг, детерминизм.
module WavSpec (tests) where

import Control.Exception (IOException, finally, try)
import Data.Bits (shiftL, (.|.))
import Data.ByteString qualified as BS
import Data.Int (Int16)
import Data.List (isInfixOf)
import Data.Vector.Unboxed qualified as U
import GHC.Float (castWord32ToFloat, double2Float)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Sound.Sig.Core
import Sound.Sig.IO
import Sound.Sig.Osc (noise)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (hClose, hFlush, readFile', stderr)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "IO" [headers, roundTrip, clipping, determinism, misc]

-- Хелперы --------------------------------------------------------------

-- | Пишет сигнал во временный файл и отдаёт байты и отчёт о клиппинге.
withWav :: Env -> BitDepth -> Sig -> (BS.ByteString -> ClipReport -> IO a) -> IO a
withWav env depth sig k = withSystemTempDirectory "hsig-test" $ \dir -> do
  let path = dir </> "t.wav"
  report <- writeWav env depth path sig
  bytes <- BS.readFile path
  k bytes report

-- | Смещение данных одной функцией: иначе тесты квантования начнут читать
-- мусор, если заголовок изменится.
headerSize :: BitDepth -> Int
headerSize Float32 = 58
headerSize _ = 44

-- | Смещение i-го сэмпла.
at :: BitDepth -> Int -> Int
at Bits16 i = headerSize Bits16 + 2 * i
at Bits24 i = headerSize Bits24 + 3 * i
at Float32 i = headerSize Float32 + 4 * i

tag :: BS.ByteString -> Int -> BS.ByteString
tag bs o = BS.take 4 (BS.drop o bs)

u8 :: BS.ByteString -> Int -> Int
u8 bs o = fromIntegral (BS.index bs o)

u16 :: BS.ByteString -> Int -> Int
u16 bs o = u8 bs o .|. (u8 bs (o + 1) `shiftL` 8)

u32 :: BS.ByteString -> Int -> Int
u32 bs o = u16 bs o .|. (u16 bs (o + 2) `shiftL` 16)

s16 :: BS.ByteString -> Int -> Int
s16 bs o = fromIntegral (fromIntegral (u16 bs o) :: Int16)

s24 :: BS.ByteString -> Int -> Int
s24 bs o
  | v >= 0x800000 = v - 0x1000000
  | otherwise = v
  where
    v = u16 bs o .|. (u8 bs (o + 2) `shiftL` 16)

f32 :: BS.ByteString -> Int -> Float
f32 bs o = castWord32ToFloat (fromIntegral (u32 bs o))

-- | Перехватывает stderr на время действия.
captureStderr :: IO a -> IO (a, String)
captureStderr act = withSystemTempFile "hsig-stderr" $ \path h -> do
  saved <- hDuplicate stderr
  hDuplicateTo h stderr
  r <-
    act `finally` do
      hFlush stderr
      hDuplicateTo saved stderr
      hClose saved
  hClose h
  msg <- readFile' path
  pure (r, msg)

-- | Тестовый Env с низкой частотой: удобно считать сэмплы руками.
slowEnv :: Env
slowEnv = defaultEnv {envRate = 4, envBlock = 3}

-- Заголовок ------------------------------------------------------------

headers :: TestTree
headers =
  testGroup
    "заголовок"
    [ testCase "16 бит" $ withWav defaultEnv Bits16 (takeSec 0.001 (constant 0)) $ \bs _ -> do
        let n = 48
        BS.length bs @?= 44 + 2 * n
        tag bs 0 @?= "RIFF"
        tag bs 8 @?= "WAVE"
        tag bs 12 @?= "fmt "
        tag bs 36 @?= "data"
        u32 bs 4 @?= 36 + 2 * n
        u32 bs 16 @?= 16
        u16 bs 20 @?= 1
        u16 bs 22 @?= 1
        u32 bs 24 @?= 48000
        u32 bs 28 @?= 48000 * 2
        u16 bs 32 @?= 2
        u16 bs 34 @?= 16
        u32 bs 40 @?= 2 * n
    , testCase "24 бита, нечётный размер данных дополняется байтом" $
        withWav slowEnv Bits24 (fromSamples (replicate 101 0)) $ \bs _ -> do
          BS.length bs @?= 44 + 303 + 1
          u32 bs 4 @?= 36 + 303 + 1
          u16 bs 34 @?= 24
          u32 bs 40 @?= 303
    , testCase "32 бита float: формат 3, chunk fact" $
        withWav slowEnv Float32 (fromSamples (replicate 10 0)) $ \bs _ -> do
          BS.length bs @?= 58 + 40
          u32 bs 16 @?= 18
          u16 bs 20 @?= 3
          u16 bs 34 @?= 32
          u16 bs 36 @?= 0
          tag bs 38 @?= "fact"
          u32 bs 46 @?= 10
          tag bs 50 @?= "data"
          u32 bs 54 @?= 40
    , testCase "пустой сигнал даёт валидный заголовок" $
        withWav slowEnv Bits16 (fromSamples []) $ \bs _ -> do
          BS.length bs @?= 44
          u32 bs 4 @?= 36
          u32 bs 40 @?= 0
    ]

-- Квантование ----------------------------------------------------------

roundTrip :: TestTree
roundTrip =
  testGroup
    "квантование"
    [ testCase "float32 пишется без изменений" $ do
        let xs = [0, 0.5, -0.25, 0.123456789, -1]
        withWav slowEnv Float32 (fromSamples xs) $ \bs _ ->
          [f32 bs (at Float32 i) | i <- [0 .. length xs - 1]] @?= map double2Float xs
    , testCase "16 бит: ошибка не больше 1.5 LSB" $ do
        let xs = [0, 0.5, -0.25, 0.123456789, -1, 0.999]
            scale = 32768 :: Double
        withWav slowEnv Bits16 (fromSamples xs) $ \bs _ -> do
          let got = [fromIntegral (s16 bs (at Bits16 i)) / scale | i <- [0 .. length xs - 1]]
          assertBool (show got) (and (zipWith (\a b -> abs (a - b) <= 1.5 / scale) xs got))
    , testCase "24 бита: ошибка не больше 1.5 LSB" $ do
        let xs = [0, 0.5, -0.25, 0.123456789, -1, 0.999]
            scale = 8388608 :: Double
        withWav slowEnv Bits24 (fromSamples xs) $ \bs _ -> do
          let got = [fromIntegral (s24 bs (at Bits24 i)) / scale | i <- [0 .. length xs - 1]]
          assertBool (show got) (and (zipWith (\a b -> abs (a - b) <= 1.5 / scale) xs got))
    , -- TPDF обязан быть двуполярным и размахом ровно 1 LSB: однополярный
      -- дизер уехал бы по среднему, а слишком громкий вышел бы за 1 LSB.
      testCase "дизер без смещения и в пределах 1 LSB" $ do
        let level = 0.25 :: Double
            n = 4000
            xs = replicate n (level / 32768)
        withWav slowEnv Bits16 (fromSamples xs) $ \bs _ -> do
          let got = [fromIntegral (s16 bs (at Bits16 i)) | i <- [0 .. n - 1]] :: [Double]
              mean = sum got / fromIntegral n
          assertBool ("среднее " <> show mean) (abs (mean - level) < 0.05)
          assertBool "выход за 1 LSB" (all (\v -> abs v <= 1) got)
          assertBool "дизер не размазывает" (maximum got > minimum got)
    , -- Регрессия: дизер и noise ходят в один генератор, и без разведения
      -- индексов дизер оказывался ровно средним двух соседних сэмплов
      -- noise с тем же seed.
      testCase "дизер не повторяет поток noise" $ do
        let n = 4000
            ns = U.fromListN (2 * n) (samples slowEnv (noise 0))
            want = [(ns U.! (2 * i) + ns U.! (2 * i + 1)) / 2 | i <- [0 .. n - 1]]
        withWav slowEnv Bits16 (fromSamples (replicate n 0)) $ \bs _ -> do
          let got = [fromIntegral (s16 bs (at Bits16 i)) | i <- [0 .. n - 1]] :: [Double]
              r = correlation got want
          assertBool ("корреляция " <> show r) (abs r < 0.1)
    ]

correlation :: [Double] -> [Double] -> Double
correlation xs ys = cov / sqrt (var xs * var ys)
  where
    n = fromIntegral (length xs)
    mean vs = sum vs / n
    var vs = sum [(v - mean vs) * (v - mean vs) | v <- vs] / n
    cov = sum (zipWith (\a b -> (a - mean xs) * (b - mean ys)) xs ys) / n

-- Клиппинг -------------------------------------------------------------

-- | Последовательно: перехват stderr глобален на процесс, а соседние тесты
-- этой же группы тоже клиппуют и пишут туда же.
clipping :: TestTree
clipping =
  sequentialTestGroup
    "клиппинг"
    AllFinish
    [ testCase "чистый сигнал: отчёт пустой" $
        withWav slowEnv Bits16 (fromSamples [0, 0.5, -0.5]) $ \_ rep -> do
          clipCount rep @?= 0
          clipFirst rep @?= Nothing
          clipPeak rep @?= 0.5
    , testCase "выход за диапазон считается и датируется" $
        withWav slowEnv Bits16 (fromSamples [0.5, 2, -3, 0.1]) $ \bs rep -> do
          clipCount rep @?= 2
          clipFirst rep @?= Just 0.25
          clipPeak rep @?= 3
          s16 bs (at Bits16 1) @?= 32767
          s16 bs (at Bits16 2) @?= -32768
    , testCase "float32 клиппинг фиксируется, но значения не режутся" $
        withWav slowEnv Float32 (fromSamples [2]) $ \bs rep -> do
          clipCount rep @?= 1
          f32 bs (at Float32 0) @?= 2
    , -- Разд. 13 прямо запрещает глотать клиппинг молча. Перехват stderr
      -- глобальный на процесс, поэтому он тут ровно один на весь набор, а
      -- чужие сообщения отсеиваются по уникальному пути файла.
      testCase "клиппинг уходит в stderr, чистый молчит" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let loud = dir </> "loud.wav"
              quiet = dir </> "quiet.wav"
          (_, msg) <- captureStderr $ do
            _ <- writeWav slowEnv Bits16 loud (fromSamples [2])
            _ <- writeWav slowEnv Bits16 quiet (fromSamples [0.5])
            pure ()
          assertBool ("нет сообщения: " <> show msg) (loud `isInfixOf` msg)
          assertBool ("слова клиппинг нет: " <> show msg) ("клиппинг" `isInfixOf` msg)
          assertBool ("чистый наследил: " <> show msg) (not (quiet `isInfixOf` msg))
    ]

-- Детерминизм ----------------------------------------------------------

determinism :: TestTree
determinism =
  testGroup
    "детерминизм"
    [ testCase "две записи одного сигнала побитово равны" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let sig = takeSec 0.01 (constant 0.3)
              a = dir </> "a.wav"
              b = dir </> "b.wav"
          _ <- writeWav defaultEnv Bits16 a sig
          _ <- writeWav defaultEnv Bits16 b sig
          xs <- BS.readFile a
          ys <- BS.readFile b
          xs @?= ys
    , testCase "envSeed меняет дизер" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let sig = takeSec 0.01 (constant (0.3 / 32768))
              a = dir </> "a.wav"
              b = dir </> "b.wav"
          _ <- writeWav defaultEnv Bits16 a sig
          _ <- writeWav defaultEnv {envSeed = 1} Bits16 b sig
          xs <- BS.readFile a
          ys <- BS.readFile b
          assertBool "дизер не зависит от seed" (xs /= ys)
    , -- Дизер индексируется номером сэмпла, а не позицией в блоке.
      testCase "размер блока не меняет файл" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let sig = takeSec 0.01 (constant (0.3 / 32768))
              a = dir </> "a.wav"
              b = dir </> "b.wav"
          _ <- writeWav defaultEnv Bits16 a sig
          _ <- writeWav defaultEnv {envBlock = 7} Bits16 b sig
          xs <- BS.readFile a
          ys <- BS.readFile b
          xs @?= ys
    ]

misc :: TestTree
misc =
  testGroup
    "прочее"
    [ testCase "недостающие каталоги создаются" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let path = dir </> "out" </> "sub" </> "t.wav"
          _ <- writeWav slowEnv Bits16 path (fromSamples [0])
          doesFileExist path >>= assertBool "файл не создан"
    , testCase "нулевая частота дискретизации это ошибка" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          r <-
            try (writeWav defaultEnv {envRate = 0} Bits16 (dir </> "t.wav") (fromSamples [0])) ::
              IO (Either IOException ClipReport)
          case r of
            Left e -> assertBool (show e) ("envRate" `isInfixOf` show e)
            Right _ -> assertFailure "ожидали ошибку"
    ]
