-- | Разбор чужой записи в величинах нашей оснастки.
--
-- Слышать я не умею, поэтому референс разбирается числами. Смысл не в том,
-- чтобы скопировать трек, а в том, чтобы снять с него статистику структуры
-- и получить, к чему целиться: сетка событий, плотность атак, шумность
-- баса, ладовое содержание и его движение, форма по громкости.
--
-- Вход - сырой поток f32le моно, его готовит tools/analyze.sh через ffmpeg.
-- Читать mp3 самим незачем: декодер это чужая задача, а разбор наш.
module Main (main) where

import Data.ByteString qualified as BS
import Data.Complex (Complex, magnitude)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Vector.Storable qualified as V
import Data.Vector.Unboxed qualified as U
import Data.Word (Word32)
import GHC.Float (castWord32ToFloat)
import Numeric.FFT.Vector.Unnormalized qualified as FFT
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Printf (printf)

-- Разбор входа ----------------------------------------------------------------

rate :: Double
rate = 44100

-- | Кадр и шаг. 2048 при 44.1 кГц это 46 мс: достаточно для разрешения по
-- низу (бин 21.5 Гц) и достаточно коротко, чтобы атаки не размазывались.
-- Шаг вчетверо меньше кадра.
frameLen, hop :: Int
frameLen = 2048
hop = 512

-- | Сырой f32le в вектор. Читается вручную, лишь бы не тащить зависимость
-- ради четырёх байт на отсчёт.
samplesOf :: BS.ByteString -> U.Vector Double
samplesOf bs = U.generate n sample
  where
    n = BS.length bs `div` 4
    sample i = realToFrac (castWord32ToFloat (word (4 * i)))
    word o =
      fromIntegral (BS.index bs o)
        + fromIntegral (BS.index bs (o + 1)) * 256
        + fromIntegral (BS.index bs (o + 2)) * 65536
        + (fromIntegral (BS.index bs (o + 3)) :: Word32) * 16777216

-- Спектр ------------------------------------------------------------------------

blackman :: U.Vector Double
blackman = U.generate frameLen $ \i ->
  let t = 2 * pi * fromIntegral i / fromIntegral (frameLen - 1)
   in 0.42 - 0.5 * cos t + 0.08 * cos (2 * t)

-- | Модули спектра кадра, начинающегося с отсчёта @off@.
frameMags :: U.Vector Double -> Int -> U.Vector Double
frameMags xs off = U.fromList (map magnitude (V.toList spec))
  where
    win = U.imap (\i w -> w * U.unsafeIndex xs (off + i)) blackman
    spec = FFT.run FFT.dftR2C (V.fromList (U.toList win)) :: V.Vector (Complex Double)

-- | Частота бина.
binHz :: Int -> Double
binHz k = fromIntegral k * rate / fromIntegral frameLen

-- | Энергия в полосе.
bandOf :: U.Vector Double -> Double -> Double -> Double
bandOf mags lo hi = U.sum (U.map (\x -> x * x) (U.slice a (b - a) mags))
  where
    a = max 0 (ceiling (lo * fromIntegral frameLen / rate))
    b = min (U.length mags) (floor (hi * fromIntegral frameLen / rate) + 1)

bands :: [(String, Double, Double)]
bands =
  [ ("20-60", 20, 60)
  , ("60-160", 60, 160)
  , ("160-400", 160, 400)
  , ("400-1200", 400, 1200)
  , ("1.2-4k", 1200, 4000)
  , ("4-15k", 4000, 15000)
  ]

-- | Спектральная плоскостность: отношение среднего геометрического к
-- среднему арифметическому. Единица это белый шум, ноль - чистый тон.
--
-- Это и есть «шумность» в измеримом виде. Для баса величина главная:
-- гулкий фактурный низ отличается от синусоиды именно ею.
flatness :: U.Vector Double -> Double -> Double -> Double
flatness mags lo hi
  | b <= a = 0
  | am <= 0 = 0
  | otherwise = exp (U.sum (U.map (log . (+ 1e-12)) part) / fromIntegral (b - a)) / am
  where
    a = max 0 (ceiling (lo * fromIntegral frameLen / rate))
    b = min (U.length mags) (floor (hi * fromIntegral frameLen / rate) + 1)
    part = U.slice a (b - a) mags
    am = U.sum part / fromIntegral (b - a)

-- | Хрома: энергия по двенадцати классам высот.
--
-- Бины от 55 Гц до 2 кГц: ниже разрешения не хватает, выше в дело идут
-- обертоны и класс размывается.
chroma :: U.Vector Double -> U.Vector Double
chroma mags = U.accum (+) (U.replicate 12 0) contribs
  where
    contribs =
      [ (pc, U.unsafeIndex mags k ** 2)
      | k <- [lo .. hi]
      , let f = binHz k
      , let pc = round (12 * logBase 2 (f / 55)) `mod` 12
      ]
    lo = max 1 (ceiling (55 * fromIntegral frameLen / rate))
    hi = min (U.length mags - 1) (floor (2000 * fromIntegral frameLen / rate))

-- Атаки --------------------------------------------------------------------------

-- | Спектральный поток: сумма приростов модулей между кадрами.
--
-- Только приросты: спад энергии это конец ноты, а не начало новой.
flux :: U.Vector Double -> U.Vector Double -> Double
flux prev cur = U.sum (U.zipWith (\a b -> max 0 (b - a)) prev cur)

-- | Пики потока выше скользящего порога.
--
-- Порог берётся от медианы окна, а не от среднего: одиночный громкий удар
-- не должен поднимать планку для соседей.
onsetsOf :: [Double] -> [Int]
onsetsOf fs =
  [ i
  | (i, v) <- zip [0 ..] fs
  , v > thresholdAt i
  , v >= maximum (window 3 i)
  , v > 0
  ]
  where
    arr = U.fromList fs
    n = U.length arr
    window w i = [U.unsafeIndex arr j | j <- [max 0 (i - w) .. min (n - 1) (i + w)]]
    thresholdAt i = 1.6 * median (window 20 i)
    median xs = let s = sortOn id xs in s !! (length s `div` 2)

-- Отчёты ---------------------------------------------------------------------------

-- | Среднее по окнам заданной длины в кадрах.
byWindow :: Int -> [Double] -> [Double]
byWindow w xs
  | null xs = []
  | otherwise = map mean (chunks w xs)
  where
    chunks k ys = if null ys then [] else take k ys : chunks k (drop k ys)
    mean ys = sum ys / fromIntegral (length ys)

db :: Double -> Double
db x = 10 * logBase 10 (max 1e-20 x)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  path <- case args of
    (p : _) -> pure p
    [] -> fail "укажите путь к сырому f32le моно"
  raw <- BS.readFile path
  let xs = samplesOf raw
      total = U.length xs
      offs = [0, hop .. total - frameLen - 1]
      mags = map (frameMags xs) offs
      secs = fromIntegral total / rate
      framesPerSec = rate / fromIntegral hop
      win10 = round (10 * framesPerSec)
  printf "длительность %.1f с, кадров %d\n\n" secs (length mags)

  putStrLn "== баланс по полосам, дБ относительно полной шкалы =="
  mapM_
    ( \(name, lo, hi) -> do
        let es = map (\m -> bandOf m lo hi) mags
        printf "%-9s средн %6.1f   пик %6.1f\n" name (db (sum es / fromIntegral (length es))) (db (maximum es))
    )
    bands

  putStrLn "\n== форма: энергия по десятисекундным окнам, дБ =="
  let totalE = map (\m -> U.sum (U.map (** 2) m)) mags
      shape = byWindow win10 totalE
  putStrLn (unwords [printf "%.0f" (db v) :: String | v <- shape])

  putStrLn "\n== шумность баса: плоскостность 20-160 Гц по окнам =="
  putStrLn "1.0 это белый шум, 0.0 чистый тон"
  let flats = byWindow win10 (map (\m -> flatness m 20 160) mags)
  putStrLn (unwords [printf "%.3f" v :: String | v <- flats])

  putStrLn "\n== атаки =="
  let fs = case mags of
        [] -> []
        (m0 : _) -> zipWith flux (m0 : mags) mags
      ons = onsetsOf fs
      iois = zipWith (-) (drop 1 ons) ons
      msOf k = fromIntegral k * 1000 / framesPerSec
  printf "всего %d, плотность %.2f в секунду\n" (length ons) (fromIntegral (length ons) / secs)
  if null iois
    then putStrLn "интервалов нет"
    else do
      let sorted = sortOn id iois
          q :: Double -> Int
          q p = sorted !! min (length sorted - 1) (floor (p * fromIntegral (length sorted)))
      printf
        "интервал между атаками, мс: медиана %.0f, квартили %.0f и %.0f\n"
        (msOf (q 0.5))
        (msOf (q 0.25))
        (msOf (q 0.75))
      let hist = take 8 (sortOn (Down . snd) (tally iois))
      putStrLn ("частые интервалы, мс: " <> unwords [printf "%.0f(x%d)" (msOf k) c :: String | (k, c) <- hist])

  putStrLn "\n== ладовое содержание по тридцатисекундным окнам =="
  putStrLn "классы высот от 55 Гц, три сильнейших в окне"
  let win30 = round (30 * framesPerSec)
      chromas = map (foldr1 (U.zipWith (+))) (chunksOf win30 (map chroma mags))
      names = ["A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"]
  mapM_
    ( \(i, c) -> do
        let top = take 3 (sortOn (Down . snd) (zip names (U.toList c)))
        printf "%3d-%3d с: %s\n" (i * 30 :: Int) ((i + 1) * 30) (unwords [n | (n, _) <- top])
    )
    (zip [0 ..] chromas)
  where
    tally xs = [(v, length (filter (== v) xs)) | v <- uniq xs]
    uniq = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
    chunksOf k ys = if null ys then [] else take k ys : chunksOf k (drop k ys)
