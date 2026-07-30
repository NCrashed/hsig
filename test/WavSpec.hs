{-# LANGUAGE OverloadedStrings #-}

-- | Запись WAV: заголовок, квантование, дизер, клиппинг, детерминизм.
module WavSpec (tests) where

import Control.Exception (ErrorCall, IOException, finally, try)
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
tests = testGroup "IO" [headers, stereo, roundTrip, reading, clipping, determinism, misc]

-- Хелперы --------------------------------------------------------------

-- | Пишет сигнал во временный файл и отдаёт байты и отчёт о клиппинге.
withWav :: Env -> BitDepth -> Sig -> (BS.ByteString -> ClipReport -> IO a) -> IO a
withWav env depth sig k = withSystemTempDirectory "hsig-test" $ \dir -> do
  let path = dir </> "t.wav"
  report <- writeWav env depth path sig
  bytes <- BS.readFile path
  k bytes report

-- | То же для стерео: чередованный поток отдаётся байтами.
withWavStereo :: Env -> BitDepth -> Sig -> Sig -> (BS.ByteString -> IO a) -> IO a
withWavStereo env depth left right k = withSystemTempDirectory "hsig-test" $ \dir -> do
  let path = dir </> "t.wav"
  _ <- writeWavStereo env depth path left right
  bytes <- BS.readFile path
  k bytes

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

-- Стерео ---------------------------------------------------------------

stereo :: TestTree
stereo =
  testGroup
    "стерео"
    [ -- Поля заголовка обязаны знать про два канала: readWav их не читает, а
      -- внешний плеер по ним и разбирает файл.
      testCase "заголовок описывает два канала" $
        withWavStereo slowEnv Bits16 (fromSamples (replicate 10 0.5)) (fromSamples (replicate 10 (-0.5))) $ \bs -> do
          u16 bs 22 @?= 2
          u32 bs 24 @?= 4
          u32 bs 28 @?= 4 * 2 * 2
          u16 bs 32 @?= 2 * 2
          u32 bs 40 @?= 10 * 2 * 2
          BS.length bs @?= 44 + 10 * 2 * 2
    , -- Кадр это левый сэмпл, потом правый. Перестановка каналов ловится
      -- только несимметричной парой.
      testCase "каналы чередуются, левый первым" $
        withWavStereo slowEnv Bits16 (fromSamples (replicate 4 0.5)) (fromSamples (replicate 4 (-0.5))) $ \bs -> do
          let frame i = (s16 bs (at Bits16 (2 * i)), s16 bs (at Bits16 (2 * i + 1)))
          mapM_
            (\i -> let (l, r) = frame i in assertBool (show (i, l, r)) (l > 16000 && r < -16000))
            [0 .. 3 :: Int]
    , -- Обрезка по короткому каналу теряла бы хвост длинного молча, поэтому
      -- короткий дополняется нулями: та же семантика, что у (+).
      testCase "короткий канал дополняется нулями" $ do
        let long = fromSamples (replicate 10 0.5)
            short = fromSamples (replicate 3 (-0.5))
            check bs (li, ri) = do
              u32 bs 40 @?= 10 * 2 * 2
              let frame i = (s16 bs (at Bits16 (2 * i)), s16 bs (at Bits16 (2 * i + 1)))
                  side pick i = pick (frame i)
              assertBool "хвост потерян" (all (\i -> abs (side li i) > 16000) [0 .. 9 :: Int])
              assertBool "короткий не дополнен" (all (\i -> abs (side ri i) <= 1) [3 .. 9 :: Int])
        withWavStereo slowEnv Bits16 long short (\bs -> check bs (fst, snd))
        withWavStereo slowEnv Bits16 short long (\bs -> check bs (snd, fst))
    , -- Границы блоков у каналов расходятся: при блоке в 3 сэмпла и длинах 10
      -- и 5 выравнивание идёт по остаткам, а не по целым блокам.
      testCase "каналы со сдвинутыми границами блоков" $
        withWavStereo slowEnv Bits16 (fromSamples (replicate 10 0.5)) (fromSamples (replicate 5 (-0.5))) $ \bs -> do
          u32 bs 40 @?= 10 * 2 * 2
          let frame i = (s16 bs (at Bits16 (2 * i)), s16 bs (at Bits16 (2 * i + 1)))
          assertBool "левый порван" (all (\i -> fst (frame i) > 16000) [0 .. 9 :: Int])
          assertBool "правый порван" (all (\i -> snd (frame i) < -16000) [0 .. 4 :: Int])
          assertBool "правый не дополнен" (all (\i -> abs (snd (frame i)) <= 1) [5 .. 9 :: Int])
    , -- Дизер у каналов независимый: индексы в чередованном потоке разные.
      -- Общий на кадр дизер выдал бы себя нулевой разностью на всех кадрах.
      testCase "дизер у каналов независимый" $ do
        let n = 2000
            s = fromSamples (replicate n (0.25 / 32768))
        withWavStereo slowEnv Bits16 s s $ \bs -> do
          let diff = [s16 bs (at Bits16 (2 * i)) - s16 bs (at Bits16 (2 * i + 1)) | i <- [0 .. n - 1]]
              differing = length (filter (/= 0) diff)
          assertBool ("различных кадров " <> show differing) (differing > n `div` 4)
          -- Дизер размахом в 1 LSB, поэтому у независимых каналов разность
          -- доходит до 2 LSB, но не больше.
          assertBool "разность больше 2 LSB" (all (\d -> abs d <= 2) diff)
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

-- Чтение ---------------------------------------------------------------

-- | Пишет сигнал, портит байты файла и отдаёт результат чтения.
readBroken :: BitDepth -> [Double] -> (BS.ByteString -> BS.ByteString) -> IO (Either IOException (Double, Int, U.Vector Double))
readBroken = readBrokenBy readWav

-- | То же, но читалку задаёт вызывающий: проверки обязаны совпадать у
-- обычного и поблочного чтения.
readBrokenBy
  :: (FilePath -> IO (Double, Int, U.Vector Double))
  -> BitDepth
  -> [Double]
  -> (BS.ByteString -> BS.ByteString)
  -> IO (Either IOException (Double, Int, U.Vector Double))
readBrokenBy reader depth xs damage = withSystemTempDirectory "hsig-test" $ \dir -> do
  let path = dir </> "t.wav"
  _ <- writeWav slowEnv depth path (fromSamples xs)
  bs <- BS.readFile path
  BS.writeFile path (damage bs)
  try (reader path)

reading :: TestTree
reading =
  testGroup
    "чтение"
    [ -- 24 бита читаются только этим путём, и ошибка расширения знака была бы
      -- видна лишь на отрицательных значениях.
      testCase "24 бита ходят туда и обратно" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let path = dir </> "t.wav"
              xs = [0, 0.5, -0.5, -1, 0.999, -0.001]
          _ <- writeWav slowEnv Bits24 path (fromSamples xs)
          (r, channels, got) <- readWav path
          r @?= 4
          channels @?= 1
          U.length got @?= length xs
          assertBool (show got) $
            and (zipWith (\a b -> abs (a - b) <= 1.5 / 8388608) xs (U.toList got))
    , -- Стык двух проверок: нечётная длина data дополняется байтом, который в
      -- размер не входит, и обе новые проверки обязаны это пережить.
      testCase "24 бита с нечётной длиной data" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let path = dir </> "t.wav"
              xs = [fromIntegral i / 200 - 0.25 | i <- [0 .. 100 :: Int]]
          _ <- writeWav slowEnv Bits24 path (fromSamples xs)
          (_, _, got) <- readWav path
          U.length got @?= 101
          assertBool (show got) $
            and (zipWith (\a b -> abs (a - b) <= 1.5 / 8388608) xs (U.toList got))
    , testCase "16 бит ходят туда и обратно" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let path = dir </> "t.wav"
              xs = [0, 0.5, -0.5, -1, 0.999]
          _ <- writeWav slowEnv Bits16 path (fromSamples xs)
          (_, _, got) <- readWav path
          assertBool (show got) $
            and (zipWith (\a b -> abs (a - b) <= 1.5 / 32768) xs (U.toList got))
    , -- Байты за концом файла читались бы как нули, то есть усечённый файл
      -- отдавал бы тишину вместо ошибки.
      testCase "усечённые данные это ошибка" $ do
        r <- readBroken Bits16 (replicate 100 0.5) (BS.take 100)
        assertLeft "усечённый файл прочитался" r
    , testCase "длина данных не кратна кадру это ошибка" $ do
        r <- readBroken Bits16 (replicate 10 0.5) (\bs -> BS.concat [BS.take 40 bs, u32le 15, BS.drop 44 bs])
        assertLeft "нечётная длина прочиталась" r
    , testCase "неизвестный формат это ошибка" $ do
        r <- readBroken Bits16 [0.5] (\bs -> BS.concat [BS.take 20 bs, BS.pack [7, 0], BS.drop 22 bs])
        assertLeft "чужой формат прочитался" r
    , -- Потоковое чтение обязано давать ровно то же, что и обычное: разница
      -- только в памяти (замер на 32 секундах: 3.7 МБ против 173).
      testCase "поблочное чтение совпадает с обычным" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let path = dir </> "t.wav"
              xs = [sin (fromIntegral i / 7) | i <- [0 .. 999 :: Int]]
          _ <- writeWav slowEnv Bits24 path (fromSamples xs)
          (r, ch, whole) <- readWav path
          (r', ch', blocks) <- readWavBlocks 128 path
          r' @?= r
          ch' @?= ch
          U.concat blocks @?= whole
          -- Все блоки по запрошенному размеру, кроме последнего.
          map U.length (init blocks) @?= replicate 7 128
          U.length (last blocks) @?= 104
    , -- Поблочное чтение обязано отвергать то же, что и обычное, и по своим
      -- путям: размер файла оно берёт из getFileSize, а шапку из первых
      -- 64 КиБ, то есть проверки идут не через те же данные.
      testCase "поблочное чтение отвергает то же, что обычное" $
        mapM_
          ( \(what, damage) -> do
              r <- readBrokenBy blocksReader Bits16 (replicate 100 0.5) damage
              assertLeft what r
          )
          [ ("усечённый файл прочитался", BS.take 100)
          , ("нечётная длина прочиталась", \bs -> BS.concat [BS.take 40 bs, u32le 15, BS.drop 44 bs])
          , ("чужой формат прочитался", \bs -> BS.concat [BS.take 20 bs, BS.pack [7, 0], BS.drop 22 bs])
          , ("не RIFF прочитался", \bs -> BS.concat ["JUNK", BS.drop 4 bs])
          ]
    , -- Чужие файлы с тегом 0xFFFE читаются по подформату, а не отвергаются.
      testCase "extensible читается по подформату" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let path = dir </> "ext.wav"
          BS.writeFile path (riffOf (extensibleFmt 1) (BS.concat [u16le 0x4000, u16le 0xC000]))
          (r, ch, xs) <- readWav path
          r @?= 4
          ch @?= 1
          U.length xs @?= 2
          assertBool (show xs) (abs (xs U.! 0 - 0.5) < 1e-4)
          assertBool (show xs) (abs (xs U.! 1 + 0.5) < 1e-4)
    , testCase "extensible с чужим подформатом это ошибка" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let path = dir </> "ext.wav"
          BS.writeFile path (riffOf (extensibleFmt 7) (u16le 0))
          try (readWav path) >>= assertLeft "чужой подформат прочитался"
    , -- Обрезанная шапка extensible не должна читаться как мусор из-за
      -- границы: GUID подформата лежит на 24-м байте.
      testCase "короткая шапка extensible это ошибка" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let path = dir </> "ext.wav"
          BS.writeFile path (riffOf (BS.take 16 (extensibleFmt 1)) (u16le 0))
          try (readWav path) >>= assertLeft "короткая шапка прочиталась"
    , testCase "не RIFF это ошибка" $ do
        r <- readBroken Bits16 [0.5] (\bs -> BS.concat ["JUNK", BS.drop 4 bs])
        assertLeft "не RIFF прочитался" r
    ]

-- | 16 бит little-endian как байты.
u16le :: Int -> BS.ByteString
u16le v = BS.take 2 (u32le v)

-- | WAV с заданной шапкой fmt и данными.
riffOf :: BS.ByteString -> BS.ByteString -> BS.ByteString
riffOf fmt body =
  BS.concat
    [ "RIFF"
    , u32le (4 + 8 + BS.length fmt + 8 + BS.length body)
    , "WAVE"
    , "fmt "
    , u32le (BS.length fmt)
    , fmt
    , "data"
    , u32le (BS.length body)
    , body
    ]

-- | Шапка WAVE_FORMAT_EXTENSIBLE: 40 байт, настоящий формат в GUID
-- подформата. Так ffmpeg пишет 24 бита и всё выше 48 кГц.
extensibleFmt :: Int -> BS.ByteString
extensibleFmt sub =
  BS.concat
    [ u16le 0xFFFE
    , u16le 1
    , u32le 4
    , u32le 8
    , u16le 2
    , u16le 16
    , u16le 22
    , u16le 16
    , u32le 0
    , u16le sub
    , BS.replicate 14 0
    ]

-- | 32 бита little-endian как байты.
u32le :: Int -> BS.ByteString
u32le v = BS.pack [fromIntegral ((v `div` (256 ^ k)) `mod` 256) | k <- [0 .. 3 :: Int]]

assertLeft :: String -> Either IOException a -> Assertion
assertLeft why r = case r of
  Left _ -> pure ()
  Right _ -> assertFailure why

-- Клиппинг -------------------------------------------------------------

-- | Последовательно: перехват stderr глобален на процесс, а соседние тесты
-- этой же группы тоже клиппуют и пишут туда же.
clipping :: TestTree
clipping =
  dependentTestGroup
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
          let broken = dir </> "broken.wav"
          (rep, msg) <- captureStderr $ do
            _ <- writeWav slowEnv Bits16 loud (fromSamples [2])
            _ <- writeWav slowEnv Bits16 quiet (fromSamples [0.5])
            writeWav slowEnv Bits16 broken (fromSamples [0 / 0, 1 / 0, 0.5])
          assertBool ("нет сообщения: " <> show msg) (loud `isInfixOf` msg)
          assertBool ("слова клиппинг нет: " <> show msg) ("клиппинг" `isInfixOf` msg)
          assertBool ("чистый наследил: " <> show msg) (not (quiet `isInfixOf` msg))
          -- NaN мимо сравнений с единицей, поэтому считается отдельно и
          -- тоже обязан быть слышен в логе.
          clipBad rep @?= 2
          clipCount rep @?= 0
          assertBool ("нет сообщения о не-числах: " <> show msg) (broken `isInfixOf` msg)
          -- Записаны нулём, но дизер поверх остаётся, поэтому в файле не
          -- строгий ноль, а тишина в пределах одного LSB.
          bytes <- BS.readFile broken
          assertBool "NaN просочился" (abs (s16 bytes (at Bits16 0)) <= 1)
          assertBool "бесконечность просочилась" (abs (s16 bytes (at Bits16 1)) <= 1)
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
    , -- Заголовок правится в самом конце, поэтому обрыв записи оставлял бы на
      -- месте пути формально валидный пустой WAV, а кэш стемов считает
      -- существующий файл готовым и молча подмешал бы тишину.
      testCase "падение записи не оставляет файла" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let path = dir </> "t.wav"
          r <-
            try (writeWav slowEnv Bits16 path (fromSamples (0.1 : error "обрыв")))
              :: IO (Either ErrorCall ClipReport)
          case r of
            Left _ -> pure ()
            Right _ -> assertFailure "ожидали ошибку"
          doesFileExist path >>= assertBool "остался пустой файл" . not
          doesFileExist (path <> ".tmp") >>= assertBool "остался временный файл" . not
    , -- Вторая половина того же контракта: неудачная перезапись не должна
      -- портить уже лежащий файл.
      testCase "падение записи не портит прежний файл" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          let path = dir </> "t.wav"
          _ <- writeWav slowEnv Bits16 path (fromSamples [0.5, 0.25])
          before <- BS.readFile path
          r <-
            try (writeWav slowEnv Bits16 path (fromSamples (0.1 : error "обрыв")))
              :: IO (Either ErrorCall ClipReport)
          case r of
            Left _ -> pure ()
            Right _ -> assertFailure "ожидали ошибку"
          BS.readFile path >>= (@?= before)
    , testCase "нулевая частота дискретизации это ошибка" $
        withSystemTempDirectory "hsig-test" $ \dir -> do
          r <-
            try (writeWav defaultEnv {envRate = 0} Bits16 (dir </> "t.wav") (fromSamples [0]))
              :: IO (Either IOException ClipReport)
          case r of
            Left e -> assertBool (show e) ("envRate" `isInfixOf` show e)
            Right _ -> assertFailure "ожидали ошибку"
    ]

-- | Поблочная читалка в интерфейсе обычной: блоки склеиваются.
blocksReader :: FilePath -> IO (Double, Int, U.Vector Double)
blocksReader path = do
  (r, ch, blocks) <- readWavBlocks 64 path
  pure (r, ch, U.concat blocks)
