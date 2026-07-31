-- | Бинауральная панорама на измеренных характеристиках (этап M10 дизайна).
--
-- 'Sound.Sig.Stereo.orbit' моделирует голову формулами и поэтому не может
-- отличить перед от зада: за это отвечают складки ушной раковины, а их
-- параметрической моделью не покрыть. Здесь вместо модели берутся измеренные
-- отклики (KEMAR, MIT Media Lab), и различение появляется само.
--
-- Данные не лежат в репозитории: путь к ним приходит переменной окружения
-- @HSIG_HRTF@, которую задаёт дев-шелл. Набор измерен на 44.1 кГц, поэтому
-- при загрузке отклики пересчитываются на частоту рендера.
--
-- Ссылка на авторов обязательна по лицензии набора: Bill Gardner и Keith
-- Martin, HRTF Measurements of a KEMAR Dummy-Head Microphone, MIT Media Lab,
-- 1994.
module Sound.Sig.HRTF
  ( Hrtf (..)
  , Dir (..)
  , loadHrtf
  , loadHrtfEnv
  , dirCount
  , dirAt
  , delayAt
  , binaural
  ) where

import Control.Monad (forM, unless)
import Control.Monad.ST (runST)
import Data.Bifunctor (bimap)
import Data.Bits (shiftL, (.|.))
import Data.ByteString qualified as BS
import Data.Int (Int16)
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Sound.Sig.Core
import Sound.Sig.Delay (vdelay)
import Sound.Sig.Resample (resample)
import Sound.Sig.Stereo (Stereo (..))
import System.Directory (doesDirectoryExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

-- | Пара откликов для одного направления. Задержка вынесена из самого
-- отклика: без этого линейная интерполяция между соседними направлениями
-- складывала бы два сдвинутых импульса и давала гребёнку вместо движения.
data Dir = Dir
  { dirLeft :: !(U.Vector Double)
  , dirRight :: !(U.Vector Double)
  , dirDelayL :: !Double
  -- ^ задержка левого уха в сэмплах частоты рендера
  , dirDelayR :: !Double
  }

-- | Набор для горизонтальной плоскости: направления идут по кругу с
-- постоянным шагом, начиная с фронта.
data Hrtf = Hrtf
  { hrtfRate :: !Double
  , hrtfDirs :: !(V.Vector Dir)
  }

-- | Сколько направлений в наборе.
dirCount :: Hrtf -> Int
dirCount = V.length . hrtfDirs

-- | Шаг измерений компактного набора KEMAR по азимуту.
azimuthStep :: Int
azimuthStep = 5

-- | Загружает горизонтальную плоскость набора и пересчитывает её на частоту
-- окружения.
--
-- Компактный набор хранит только правое полушарие (азимуты 0..180): левое
-- получается перестановкой ушей, потому что голова симметрична. Так же
-- рекомендуют делать сами авторы набора.
loadHrtf :: Env -> FilePath -> IO Hrtf
loadHrtf env root = do
  let dir = root </> "elev0"
  ok <- doesDirectoryExist dir
  unless ok (ioError (userError (dir <> ": нет каталога elev0, это не набор KEMAR")))
  half <- forM [0, azimuthStep .. 180] $ \az -> do
    bytes <- BS.readFile (dir </> fileName az)
    pure (splitStereo bytes)
  let measured = map (bimap (prep env) (prep env)) half
      -- Зеркало: источник слева это тот же отклик с переставленными ушами.
      mirrored = reverse (drop 1 (init measured))
      round' = measured <> map (\(l, r) -> (r, l)) mirrored
  pure
    Hrtf
      { hrtfRate = envRate env
      , hrtfDirs = V.fromList [Dir (snd l) (snd r) (fst l) (fst r) | (l, r) <- round']
      }
  where
    fileName az = "H0e" <> pad (show az) <> "a.dat"
    pad s = replicate (3 - length s) '0' <> s

-- | То же по пути из переменной окружения @HSIG_HRTF@.
loadHrtfEnv :: Env -> IO Hrtf
loadHrtfEnv env = do
  path <- lookupEnv "HSIG_HRTF"
  case path of
    Just p -> loadHrtf env p
    Nothing ->
      ioError
        ( userError
            "hsig: не задан HSIG_HRTF. Набор KEMAR приезжает через flake, войдите в nix develop"
        )

-- | Отсчёты файла: 16 бит, старший байт первым, чередование левое-правое.
splitStereo :: BS.ByteString -> (U.Vector Double, U.Vector Double)
splitStereo bytes = (grab 0, grab 1)
  where
    frames = BS.length bytes `div` 4
    grab side = U.generate frames (\i -> sample (2 * i + side))
    sample i = fromIntegral (int16At (2 * i)) / 32768
    int16At :: Int -> Int16
    int16At o = fromIntegral (hi `shiftL` 8 .|. lo :: Int)
      where
        hi = fromIntegral (BS.index bytes o)
        lo = fromIntegral (BS.index bytes (o + 1))

-- | Пересчитывает отклик на частоту рендера и выносит из него задержку.
prep :: Env -> U.Vector Double -> (Double, U.Vector Double)
prep env ir = (fromIntegral k, U.drop k resampled)
  where
    resampled = render env (resample 44100 (fromSamples (U.toList ir)))
    k = onsetOf resampled

-- | Начало отклика: первый переход через пятую часть пика. Порог именно по
-- пику, а не по абсолютной величине: отклики дальнего уха тише ближнего в
-- разы, и общий порог срезал бы у них начало.
onsetOf :: U.Vector Double -> Int
onsetOf xs
  | U.null xs = 0
  | otherwise = fromMaybe 0 (U.findIndex (\v -> abs v >= level) xs)
  where
    level = 0.2 * U.maximum (U.map abs xs)

-- | Направление по азимуту в радианах: интерполяция между двумя ближайшими
-- измерениями. Ноль это фронт, положительный угол вправо.
dirAt :: Hrtf -> Double -> (U.Vector Double, U.Vector Double)
dirAt h angle = (blend dirLeft, blend dirRight)
  where
    (i, j, w) = neighbours h angle
    dirs = hrtfDirs h
    blend f = U.zipWith (\a b -> (1 - w) * a + w * b) (pad (f (dirs V.! i))) (pad (f (dirs V.! j)))
    n = maximum [U.length (dirLeft d) `max` U.length (dirRight d) | d <- [dirs V.! i, dirs V.! j]]
    pad v = v U.++ U.replicate (n - U.length v) 0

-- | Задержки ушей для угла, в сэмплах. Интерполируются отдельно от откликов,
-- поэтому межушная разница меняется плавно.
delayAt :: Hrtf -> Double -> (Double, Double)
delayAt h angle = (tween dirDelayL, tween dirDelayR)
  where
    (i, j, w) = neighbours h angle
    dirs = hrtfDirs h
    tween f = (1 - w) * f (dirs V.! i) + w * f (dirs V.! j)

-- | Соседние направления сетки и вес второго.
neighbours :: Hrtf -> Double -> (Int, Int, Double)
neighbours h angle = (i `mod` n, (i + 1) `mod` n, w)
  where
    n = dirCount h
    turns = angle / (2 * pi)
    pos = (turns - fromIntegral (floor turns :: Int)) * fromIntegral n
    i = floor pos
    w = pos - fromIntegral i

-- | Панорама по измеренным откликам: угол в радианах, ноль это фронт,
-- положительный угол вправо.
--
-- Цена: источник считается дважды, по разу на ухо, и на каждый выходной
-- сэмпл идёт свёртка с откликом в полторы сотни отводов. Оберните источник в
-- 'share', иначе он посчитается ещё и дважды сам.
--
-- Отклик пересчитывается на каждом блоке и переходит к новому линейно внутри
-- блока: скачок отклика на движущемся источнике слышен щелчком.
binaural :: Hrtf -> Sig -> Sig -> Stereo
binaural h angle src = Stereo (ear LeftEar) (ear RightEar)
  where
    ear side = delayed side (convolved side)
    convolved side = Sig $ \env -> convolve h side (runSig angle env) (runSig src env)
    delayed side =
      vdelay maxDelaySec (mapSig (\a -> (base + ofSide side (delayAt h a)) / hrtfRate h) angle)
    -- Дробная задержка не умеет меньше половины ядра, поэтому обоим ушам
    -- добавляется общая полка: разница между ушами от неё не меняется.
    base = 8

maxDelaySec :: Double
maxDelaySec = 0.005

-- | Какое ухо: свёртка и задержка выбирают из пары одинаково.
data Side = LeftEar | RightEar

ofSide :: Side -> (a, a) -> a
ofSide LeftEar = fst
ofSide RightEar = snd

-- | Свёртка по блокам с линейным переходом между откликами соседних блоков.
convolve :: Hrtf -> Side -> Chunks -> Chunks -> Chunks
convolve h side = go (pick (dirAt h 0)) (U.replicate taps 0)
  where
    pick = ofSide side
    taps = U.length (dirLeft (V.head (hrtfDirs h)))
    go _ _ [] _ = []
    go _ _ _ [] = []
    go prev history (a : as) (x : xs) = out : go now rest as xs
      where
        now = pick (dirAt h (if U.null a then 0 else U.head a))
        (out, rest) = convBlock prev now history x
    convBlock prev now history x = runST $ do
      let n = U.length x
          m = U.length history
      buf <- UM.new n
      let at i k
            | i - k >= 0 = U.unsafeIndex x (i - k)
            | m + (i - k) >= 0 = U.unsafeIndex history (m + (i - k))
            | otherwise = 0
          taps' = min (U.length prev) (U.length now)
          step i
            | i >= n = pure ()
            | otherwise = do
                let t = if n <= 1 then 1 else fromIntegral i / fromIntegral (n - 1)
                    acc k s
                      | k >= taps' = s
                      | otherwise =
                          acc
                            (k + 1)
                            (s + at i k * ((1 - t) * U.unsafeIndex prev k + t * U.unsafeIndex now k))
                UM.unsafeWrite buf i (acc 0 0)
                step (i + 1)
      step 0
      out <- U.unsafeFreeze buf
      let keep = U.length prev
          joined = history U.++ x
          rest = U.drop (U.length joined - keep) joined
      pure (out, rest)
