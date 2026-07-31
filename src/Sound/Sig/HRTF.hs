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
  , planeCount
  , elevations
  , dirAt
  , delayAt
  , binaural
  ) where

import Control.Monad (forM, unless, when)
import Control.Monad.ST (runST)
import Data.Bifunctor (bimap)
import Data.Bits (shiftL, (.|.))
import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.Int (Int16)
import Data.List (sortOn, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Sound.Sig.Core
import Sound.Sig.Delay (vdelay)
import Sound.Sig.Resample (resample)
import Sound.Sig.Stereo (Stereo (..))
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.Read (readMaybe)

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

-- | Набор для полусферы: плоскости по элевациям, в каждой свой шаг по
-- азимуту.
data Plane = Plane
  { planeElev :: !Double
  -- ^ элевация в градусах
  , planeAz :: !(U.Vector Double)
  -- ^ азимуты по кругу в градусах, по возрастанию
  , planeDirs :: !(V.Vector Dir)
  }

data Hrtf = Hrtf
  { hrtfRate :: !Double
  , hrtfPlanes :: !(V.Vector Plane)
  -- ^ по возрастанию элевации
  }

-- | Сколько направлений в наборе всего.
dirCount :: Hrtf -> Int
dirCount h = sum [V.length (planeDirs p) | p <- V.toList (hrtfPlanes h)]

-- | Сколько плоскостей (элеваций).
planeCount :: Hrtf -> Int
planeCount = V.length . hrtfPlanes

-- | Элевации набора, по возрастанию.
elevations :: Hrtf -> [Double]
elevations h = [planeElev p | p <- V.toList (hrtfPlanes h)]

-- | Загружает всю полусферу набора и пересчитывает её на частоту окружения.
--
-- Сетка неравномерная: внизу измерений много (шаг 5 градусов), к зениту всё
-- меньше, а в самом зените одно направление. Поэтому азимуты читаются из
-- имён файлов, а не считаются шагом.
--
-- Компактный набор хранит только правое полушарие (азимуты 0..180): левое
-- получается перестановкой ушей, потому что голова симметрична. Так же
-- рекомендуют делать сами авторы набора.
loadHrtf :: Env -> FilePath -> IO Hrtf
loadHrtf env root = do
  dirs <- listDirectory root
  let elevs = sortOn fst [(e, root </> d) | d <- dirs, Just e <- [elevOf d]]
  when (null elevs) (ioError (userError (root <> ": нет каталогов elevNN, это не набор KEMAR")))
  planes <- mapM (loadPlane env) elevs
  pure Hrtf {hrtfRate = envRate env, hrtfPlanes = V.fromList planes}
  where
    elevOf d = stripPrefix "elev" d >>= readMaybe

-- | Одна плоскость: измеренные азимуты плюс их зеркала.
loadPlane :: Env -> (Double, FilePath) -> IO Plane
loadPlane env (elev, dir) = do
  files <- listDirectory dir
  let measured = sortOn fst [(az, dir </> f) | f <- files, Just az <- [azimuthOf f]]
  loaded <- forM measured $ \(az, path) -> do
    bytes <- BS.readFile path
    let (l, r) = splitStereo bytes
    pure (az, Dir (snd (prep env l)) (snd (prep env r)) (fst (prep env l)) (fst (prep env r)))
  -- Зеркало: источник слева это тот же отклик с переставленными ушами.
  -- Азимуты 0 и 180 сами себе зеркало, их не дублируем.
  let mirrored =
        [ (360 - az, d {dirLeft = dirRight d, dirRight = dirLeft d, dirDelayL = dirDelayR d, dirDelayR = dirDelayL d})
        | (az, d) <- loaded
        , az > 0 && az < 180
        ]
      full = sortOn fst (loaded <> mirrored)
  pure
    Plane
      { planeElev = elev
      , planeAz = U.fromList (map fst full)
      , planeDirs = V.fromList (map snd full)
      }

-- | Азимут из имени файла вида @H0e045a.dat@.
azimuthOf :: FilePath -> Maybe Double
azimuthOf name = do
  rest <- stripPrefix "H" name
  let (_, afterE) = break (== 'e') rest
  digits <- stripPrefix "e" afterE
  readMaybe (takeWhile isDigit digits)

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

-- | Отклики для направления: азимут и элевация в радианах. Ноль азимута это
-- фронт, положительный вправо; ноль элевации это горизонт, положительная
-- вверх.
--
-- Билинейная интерполяция: сперва по азимуту внутри двух ближайших
-- плоскостей, потом между ними. Вне измеренного диапазона элевация
-- зажимается: снизу набор кончается на -40 градусах, сверху зенитом.
dirAt :: Hrtf -> Double -> Double -> (U.Vector Double, U.Vector Double)
dirAt h az el = (blend fst, blend snd)
  where
    (pa, pb, w) = planesAt h el
    lower = planeDir pa az
    upper = planeDir pb az
    blend side = mixIr w (side lower) (side upper)

-- | Задержки ушей для направления, в сэмплах. Интерполируются отдельно от
-- откликов, поэтому межушная разница меняется плавно.
delayAt :: Hrtf -> Double -> Double -> (Double, Double)
delayAt h az el = (tween fst, tween snd)
  where
    (pa, pb, w) = planesAt h el
    lower = planeDelay pa az
    upper = planeDelay pb az
    tween side = (1 - w) * side lower + w * side upper

-- | Отклики внутри одной плоскости.
planeDir :: Plane -> Double -> (U.Vector Double, U.Vector Double)
planeDir p az = (mixIr w (dirLeft a) (dirLeft b), mixIr w (dirRight a) (dirRight b))
  where
    (a, b, w) = aroundAz p az

-- | Задержки внутри одной плоскости.
planeDelay :: Plane -> Double -> (Double, Double)
planeDelay p az = ((1 - w) * dirDelayL a + w * dirDelayL b, (1 - w) * dirDelayR a + w * dirDelayR b)
  where
    (a, b, w) = aroundAz p az

-- | Линейная смесь двух откликов, короткий добивается нулями.
mixIr :: Double -> U.Vector Double -> U.Vector Double -> U.Vector Double
mixIr w a b = U.generate n (\i -> (1 - w) * at a i + w * at b i)
  where
    n = max (U.length a) (U.length b)
    at v i = if i < U.length v then U.unsafeIndex v i else 0

-- | Два соседних направления плоскости по азимуту и вес второго.
aroundAz :: Plane -> Double -> (Dir, Dir, Double)
aroundAz p az
  | n == 0 = error "hsig: пустая плоскость HRTF"
  | n == 1 = (dirs V.! 0, dirs V.! 0, 0)
  | otherwise = (dirs V.! i, dirs V.! j, w)
  where
    dirs = planeDirs p
    azs = planeAz p
    n = V.length dirs
    deg = wrapDeg (az * 180 / pi)
    i = lastBelow 0 (n - 1)
    j = (i + 1) `mod` n
    -- Последний азимут не меньше искомого: двоичного поиска не надо, сетка
    -- маленькая, зато код очевиден.
    lastBelow acc k
      | k < 0 = acc
      | U.unsafeIndex azs k <= deg = k
      | otherwise = lastBelow acc (k - 1)
    a0 = U.unsafeIndex azs i
    a1 = let v = U.unsafeIndex azs j in if v > a0 then v else v + 360
    w = if a1 > a0 then (deg - a0) / (a1 - a0) else 0

-- | Угол в градусах, приведённый к [0, 360).
wrapDeg :: Double -> Double
wrapDeg d = d - 360 * fromIntegral (floor (d / 360) :: Int)

-- | Две соседние плоскости по элевации и вес верхней.
planesAt :: Hrtf -> Double -> (Plane, Plane, Double)
planesAt h el
  | deg <= planeElev first = (first, first, 0)
  | deg >= planeElev last' = (last', last', 0)
  | otherwise = go 0
  where
    ps = hrtfPlanes h
    n = V.length ps
    first = ps V.! 0
    last' = ps V.! (n - 1)
    deg = el * 180 / pi
    go k
      | k >= n - 1 = (last', last', 0)
      | planeElev (ps V.! (k + 1)) >= deg =
          let a = ps V.! k
              b = ps V.! (k + 1)
              span' = planeElev b - planeElev a
           in (a, b, if span' > 0 then (deg - planeElev a) / span' else 0)
      | otherwise = go (k + 1)

-- | Панорама по измеренным откликам: азимут и элевация в радианах.
--
-- Азимут: ноль это фронт, положительный вправо. Элевация: ноль это
-- горизонт, положительная вверх; набор измерен от -40 до 90 градусов, за
-- границами значение зажимается.
--
-- Цена: источник считается дважды, по разу на ухо, и на каждый выходной
-- сэмпл идёт свёртка с откликом в полторы сотни отводов. Оберните источник в
-- 'share', иначе он посчитается ещё и дважды сам.
--
-- Отклик пересчитывается на каждом блоке и переходит к новому линейно внутри
-- блока: скачок отклика на движущемся источнике слышен щелчком.
binaural :: Hrtf -> Sig -> Sig -> Sig -> Stereo
binaural h az el src = Stereo (ear LeftEar) (ear RightEar)
  where
    ear side = delayed side (convolved side)
    convolved side = Sig $ \env -> convolve h side (runSig az env) (runSig el env) (runSig src env)
    delayed side =
      vdelay
        maxDelaySec
        (zipChunks Truncate (\a e -> (base + ofSide side (delayAt h a e)) / hrtfRate h) az el)
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
convolve :: Hrtf -> Side -> Chunks -> Chunks -> Chunks -> Chunks
convolve h side = go (pick (dirAt h 0 0)) (U.replicate taps 0)
  where
    pick = ofSide side
    taps = U.length (dirLeft (V.head (planeDirs (V.head (hrtfPlanes h)))))
    go _ _ [] _ _ = []
    go _ _ _ [] _ = []
    go _ _ _ _ [] = []
    go prev history (a : as) (e : es) (x : xs) = out : go now rest as es xs
      where
        now = pick (dirAt h (headOr 0 a) (headOr 0 e))
        (out, rest) = convBlock prev now history x
    headOr d v = if U.null v then d else U.head v
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
