-- | Ресемплинг и оверсэмплинг.
--
-- FIR проектируется оконным sinc с окном Кайзера, реализация полифазная
-- (разд. 6.4 дизайна). Задержка компенсируется внутри oversample: наружу
-- он прозрачен по времени, иначе параллельные ветви графа разъедутся и в
-- миксе появится гребёнчатая фильтрация.
module Sound.Sig.Resample
  ( oversample
  , stageFilter
  , kaiserLowpass
  , kaiserBeta
  , besselI0
  ) where

import Data.Vector.Unboxed qualified as U
import Sound.Sig.Core

-- Проектирование FIR --------------------------------------------------------

-- | Модифицированная функция Бесселя первого рода нулевого порядка.
besselI0 :: Double -> Double
besselI0 x = go 1 1 1
  where
    q = x * x / 4
    go !acc !term !k
      | t < 1e-18 * acc = acc + t
      | otherwise = go (acc + t) t (k + 1)
      where
        t = term * q / fromIntegral (k * k :: Int)

-- | Параметр окна Кайзера по требуемому затуханию в полосе задерживания.
kaiserBeta :: Double -> Double
kaiserBeta a
  | a > 50 = 0.1102 * (a - 8.7)
  | a >= 21 = 0.5842 * (a - 21) ** 0.4 + 0.07886 * (a - 21)
  | otherwise = 0

-- | ФНЧ методом оконного sinc: n отводов, срез fc в долях частоты
-- дискретизации, затухание atten дБ. Усиление на постоянном токе ровно 1.
kaiserLowpass :: Int -> Double -> Double -> U.Vector Double
kaiserLowpass n fc atten = U.map (/ U.sum weighted) weighted
  where
    beta = kaiserBeta atten
    mid = fromIntegral (n - 1) / 2
    weighted = U.generate n $ \i ->
      let r = fromIntegral i / mid - 1
          w = besselI0 (beta * sqrt (max 0 (1 - r * r))) / besselI0 beta
       in w * sinc (2 * fc * (fromIntegral i - mid))
    sinc t
      | t == 0 = 1
      | otherwise = sin (pi * t) / (pi * t)

-- Оверсэмплинг --------------------------------------------------------------

-- | Затухание в полосе задерживания. Разд. 6.4 требует не меньше 120 дБ,
-- берём с запасом: null-тест разд. 12 требует -140 dBFS, а пульсации двух
-- стадий складываются.
attenDb :: Double
attenDb = 160

-- | Отводов на стадию: около 64 на кратность. Число нечётное, поэтому
-- задержка (N-1)/2 целая, а суммарная по двум стадиям делится на кратность.
tapsFor :: Int -> Int
tapsFor n = 64 * n + 1

-- | Фильтр одной стадии. Экспортируется, чтобы приёмочные проверки мерили
-- ровно то, что стоит в тракте, а не копию его параметров.
stageFilter :: Int -> U.Vector Double
stageFilter n = kaiserLowpass (tapsFor n) (0.5 / fromIntegral n) attenDb

-- | Поднимает частоту дискретизации для поддерева графа: интерполяция в n
-- раз, обработка на высокой частоте, децимация обратно.
--
-- Компенсация задержки это дополнение входа нулями и срез такого же куска
-- на выходе, поэтому fx внутри ожидается сохраняющим длину. Если он
-- укорачивает сигнал, выход окажется короче ещё на задержку; выравнивание
-- по времени при этом не страдает.
oversample :: Int -> Fx -> Fx
oversample n fx sig
  | n <= 1 = fx sig
  | otherwise = Sig $ \env ->
      let block = blockOf env
          envHi = env {envRate = envRate env * fromIntegral n, envBlock = block * n}
          h = stageFilter n
          -- Вставка нулей делит амплитуду на n, интерполятор её возвращает.
          hUp = U.map (* fromIntegral n) h
          delay = (tapsFor n - 1) `div` n
          -- FIR отдаёт сигнал с задержкой, поэтому конечный вход
          -- дополняется нулями ровно на неё. Бесконечного это не касается.
          padded = rechunk block (runSig sig env <> [U.replicate delay 0])
          hiChunks = rechunk (block * n) (interpolate n hUp padded)
          processed = runSig (fx (Sig (const hiChunks))) envHi
       in rechunk block (dropSamples delay (decimate n h processed))

-- | Полифазная интерполяция в n раз: для каждого входного сэмпла n выходных,
-- каждый своей ветвью фильтра.
interpolate :: Int -> U.Vector Double -> Chunks -> Chunks
interpolate n h = go (U.replicate (k - 1) 0)
  where
    (k, phases) = polyphase n h
    go _ [] = []
    go hist (c : cs) = out : go (U.drop m ext) cs
      where
        m = U.length c
        ext = hist U.++ c
        out = U.generate (m * n) $ \i ->
          let (t, p) = i `divMod` n
              base = p * k
           in dot k (\j -> U.unsafeIndex phases (base + j) * U.unsafeIndex ext (k - 1 + t - j))

-- | Децимация в n раз: считаем только каждый n-й выход. Смещение живёт в
-- состоянии, поэтому блок необязан делиться на n. Инвариант блоков сейчас
-- гарантирует делимость всем блокам кроме последнего, так что ветка со
-- смещением это страховка на случай нестандартного потока.
decimate :: Int -> U.Vector Double -> Chunks -> Chunks
decimate n h = go (U.replicate (len - 1) 0) 0
  where
    len = U.length h
    go _ _ [] = []
    go hist off (c : cs)
      | count <= 0 = go rest off' cs
      | otherwise = out : go rest off' cs
      where
        m = U.length c
        ext = hist U.++ c
        rest = U.drop m ext
        count = if off >= m then 0 else (m - off - 1) `div` n + 1
        off' = off + count * n - m
        out = U.generate count $ \t ->
          let e = len - 1 + off + t * n
           in dot len (\j -> U.unsafeIndex h j * U.unsafeIndex ext (e - j))

-- | Раскладка фильтра по n ветвям: ветвь p это отводы h[j*n + p]. Хвост
-- дополняется нулями, чтобы все ветви были одной длины.
polyphase :: Int -> U.Vector Double -> (Int, U.Vector Double)
polyphase n h = (k, U.generate (n * k) pick)
  where
    k = (U.length h + n - 1) `div` n
    pick i =
      let (p, j) = i `divMod` k
          idx = j * n + p
       in if idx < U.length h then U.unsafeIndex h idx else 0

dot :: Int -> (Int -> Double) -> Double
dot n f = go 0 0
  where
    go !acc !i
      | i >= n = acc
      | otherwise = go (acc + f i) (i + 1)

dropSamples :: Int -> Chunks -> Chunks
dropSamples n cs
  | n <= 0 = cs
  | otherwise = case cs of
      [] -> []
      c : rest
        | U.length c > n -> U.drop n c : rest
        | otherwise -> dropSamples (n - U.length c) rest
