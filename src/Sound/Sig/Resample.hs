-- | Ресемплинг и оверсэмплинг.
--
-- FIR проектируется оконным sinc с окном Кайзера, реализация полифазная
-- (разд. 6.4 дизайна). Задержка компенсируется внутри oversample: наружу
-- он прозрачен по времени, иначе параллельные ветви графа разъедутся и в
-- миксе появится гребёнчатая фильтрация.
module Sound.Sig.Resample
  ( oversample
  , resample
  , stageFilter
  , kaiserLowpass
  , kaiserBeta
  , besselI0
  ) where

import Data.Vector.Unboxed qualified as U
import Sound.Sig.Core

-- Смена частоты дискретизации -----------------------------------------------

-- | Читает сигнал так, будто он посчитан на частоте @from@, и приводит к
-- частоте рендера. Отношение произвольное, не только целое.
--
-- Ядро то же, что у дробной задержки: оконный sinc, таблица фаз. При
-- понижении частоты sinc сжимается по частоте до новой полосы Найквиста и
-- растягивается по времени, то есть служит и интерполятором, и фильтром
-- против наложения - отдельного ФНЧ не нужно. Число отводов растёт вместе с
-- коэффициентом понижения, поэтому качество от него не зависит.
--
-- Задержки не вносит: ядро центрировано на целевой позиции, выходной сэмпл
-- m соответствует входному времени @m * from \/ rate@. За краями входа
-- предполагается тишина.
--
-- Работает потоково: входа подтягивается ровно столько, сколько нужно
-- очередному блоку выхода, поэтому бесконечный вход даёт бесконечный выход,
-- а память не растёт. Знать длину входа заранее нельзя - на бесконечном
-- сигнале это повисло бы.
--
-- Замер на понижении 96 -> 48 кГц: в полосе пропускания до 20 кГц отклонение
-- не измеряется (меньше 0.001 дБ), в полосе задерживания -163 дБ на 28 кГц
-- и -185 дБ выше 36 кГц. Переходная полоса около 4 кГц: то, что лежит между
-- 24 и 28 кГц, заворачивается в 20-24 кГц с ослаблением около 68 дБ.
--
-- Целые отношения - лёгкий случай: там дробная часть попадает ровно в строку
-- таблицы ядер. Замер на неудобном 44.1 -> 48 кГц: полоса пропускания до
-- 15 кГц ровная до 0.0001 дБ, худший посторонний тон -173 дБ на 1 кГц и
-- -126 дБ на 15 кГц. Требование разд. 12 (0.01 дБ и -120 дБ) выполнено на
-- обоих.
resample :: Double -> Sig -> Sig
resample from sig
  | from <= 0 = error "hsig: resample требует положительной частоты"
  | otherwise = Sig $ \env ->
      let stepIn = from / envRate env
          -- Полоса относительно входной частоты: при повышении берём всю,
          -- при понижении ужимаем до целевого Найквиста.
          band = min 1 (1 / stepIn)
          -- Отводов надо много: у окна Кайзера на 90 дБ переходная полоса
          -- обратно пропорциональна их числу, и на 64 отводах она шире
          -- октавы - то, что чуть выше новой полосы, заворачивалось бы почти
          -- неослабленным. На 128 переходная полоса около 4 кГц.
          half = max 48 (ceiling (48 / band))
          block = blockOf env
       in rechunk
            block
            ( resampleGo
                stepIn
                half
                (fracKernels band half)
                block
                0
                (Carry U.empty 0 Nothing)
                (runSig sig env {envRate = from})
            )

-- | Подтянутый кусок входа: сэмплы, абсолютный индекс их начала и длина
-- входа, если он уже кончился.
data Carry = Carry !(U.Vector Double) !Int !(Maybe Int)

resampleGo :: Double -> Int -> U.Vector Double -> Int -> Int -> Carry -> Chunks -> Chunks
resampleGo step half table block o (Carry buf base end) src
  | m <= 0 = []
  | otherwise = out : resampleGo step half table block (o + m) carry' src'
  where
    -- Последний входной отсчёт, нужный этому блоку выхода.
    need = floor (fromIntegral (o + block - 1) * step) + half
    (buf', base', end', src') = fill need buf base end src
    -- Выход кончается там же, где вход: дальше ядру нечего читать.
    m = case end' of
      Just n -> min block (max 0 (ceiling (fromIntegral n / step) - o))
      Nothing -> block
    out = U.generate m (\i -> resampleAt step half table buf' base' (o + i))
    -- Начало буфера, которое следующему блоку уже не понадобится.
    firstNeed = floor (fromIntegral (o + m) * step) - half + 1
    keep = max 0 (min (U.length buf') (firstNeed - base'))
    carry' = Carry (U.drop keep buf') (base' + keep) end'

-- | Тянет вход, пока буфер не покроет нужный индекс или пока вход не кончится.
fill :: Int -> U.Vector Double -> Int -> Maybe Int -> Chunks -> (U.Vector Double, Int, Maybe Int, Chunks)
fill need buf base end src
  | base + U.length buf > need = (buf, base, end, src)
  | otherwise = case src of
      [] -> (buf, base, Just (base + U.length buf), [])
      c : cs -> fill need (buf U.++ c) base end cs

-- | Один выходной сэмпл: ядро нужной фазы поверх окна входа.
--
-- Отводы берутся с интерполяцией между двумя соседними строками таблицы.
-- Без неё выход считался бы для времени, округлённого до шага таблицы, а
-- это округление гуляет от сэмпла к сэмплу - то есть модуляция времени
-- пилой, и в спектре образы на уровне около -60 дБ. Заметно это только на
-- неудобных отношениях: при целом отношении дробная часть попадает ровно в
-- строку, и ошибки нет вовсе.
resampleAt :: Double -> Int -> U.Vector Double -> U.Vector Double -> Int -> Int -> Double
resampleAt step half table buf base m = go 0 0
  where
    taps = 2 * half
    t = fromIntegral m * step
    k = floor t :: Int
    q = (t - fromIntegral k) * fromIntegral fracSteps
    p = min (fracSteps - 1) (floor q)
    g = q - fromIntegral p
    lo = p * taps
    hi = lo + taps
    go !j !acc
      | j >= taps = acc
      | otherwise = go (j + 1) (acc + h * at (k - half + 1 + j))
      where
        h0 = U.unsafeIndex table (lo + j)
        h1 = U.unsafeIndex table (hi + j)
        h = h0 + g * (h1 - h0)
    at i
      | i < base || i >= base + U.length buf = 0
      | otherwise = U.unsafeIndex buf (i - base)

-- | Фаз в таблице ядер. Строк на одну больше: последней нужен сосед справа.
fracSteps :: Int
fracSteps = 512

-- | Таблица ядер: на каждую фазу свой sinc, сжатый до полосы band и
-- обрезанный окном Кайзера. Сумма отводов нормирована в единицу, иначе
-- постоянная составляющая зависела бы от фазы.
fracKernels :: Double -> Int -> U.Vector Double
fracKernels band half = U.concat (map row [0 .. fracSteps])
  where
    taps = 2 * half
    beta = kaiserBeta attenDb
    row p = U.map (/ total) ts
      where
        f = fromIntegral p / fromIntegral fracSteps
        ts = U.generate taps (\t -> tap (fromIntegral (t - half + 1) - f))
        total = U.sum ts
    tap x = sinc (band * x) * window (x / fromIntegral half)
    sinc x
      | abs x < 1e-12 = 1
      | otherwise = sin (pi * x) / (pi * x)
    window r
      | abs r >= 1 = 0
      | otherwise = besselI0 (beta * sqrt (1 - r * r)) / besselI0 beta

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
