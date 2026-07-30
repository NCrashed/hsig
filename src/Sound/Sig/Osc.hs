-- | Осцилляторы.
--
-- Волны аддитивные: гармоники суммируются честно до Найквиста, никаких
-- naive saw и PolyBLEP. Дорого и единственно точно (разд. 2 и 6.2 дизайна).
module Sound.Sig.Osc
  ( phase
  , sine
  , saw
  , square
  , pulse
  , tri
  , noise
  , partialGain
  ) where

import Data.Vector.Unboxed qualified as U
import Sound.Sig.Block (align2)
import Sound.Sig.Core
import Sound.Sig.Random (doubleAt)

twoPi :: Double
twoPi = 2 * pi

-- | Приведение к [0, 2*pi): без него Double теряет точность на длинных нотах.
wrapPhase :: Double -> Double
wrapPhase p = p - twoPi * fromIntegral (floor (p / twoPi) :: Int)

-- | Фазы блока и перенос на следующий: scanl' даёт n+1 значений, последнее
-- это состояние.
phaseChunk :: Double -> Double -> U.Vector Double -> (U.Vector Double, Double)
phaseChunk k p0 c = (U.init acc, U.last acc)
  where
    acc = U.scanl' (\p f -> wrapPhase (p + k * f)) p0 c

-- | Частота в накопленную фазу: p[n] = p[n-1] + 2*pi*f[n]/sr, старт с нуля.
phase :: Sig -> Sig
phase (Sig g) = Sig $ \env ->
  let k = twoPi / envRate env
      go _ [] = []
      go !p (c : cs) = let (out, p') = phaseChunk k p c in out : go p' cs
   in go 0 (g env)

-- | Синус заданной частоты.
sine :: Sig -> Sig
sine = mapSig sin . phase

-- Аддитивные волны -------------------------------------------------------

-- | Плавный сброс верхних гармоник: raised cosine на последней октаве до
-- Найквиста, аргумент это k*f/nyquist.
--
-- При свипе число гармоник меняется, и уходящая за Найквист гармоника без
-- этого спада пропадала бы скачком, то есть щелчком.
partialGain :: Double -> Double
partialGain r
  | r <= 0.5 = 1
  | r >= 1 = 0
  | otherwise = 0.5 * (1 + cos (pi * (2 * r - 1)))

-- | Потолок числа гармоник. Страхует от переполнения при частоте около нуля
-- и от рендера длиной в вечность; при 48 кГц полностью покрывает всё от 3 Гц.
maxPartials :: Int
maxPartials = 8192

-- | Осциллятор из амплитуд гармоник: amp k это множитель при sin (k*phi),
-- ноль означает отсутствующую гармонику.
--
-- Фаза считается здесь же, а не через phase: иначе частота вычислялась бы
-- дважды.
additive :: (Int -> Double) -> Sig -> Sig
additive amp (Sig g) = Sig $ \env ->
  let k = twoPi / envRate env
      nyq = envRate env / 2
      go _ [] = []
      go !p (c : cs) =
        let (ps, p') = phaseChunk k p c
         in U.zipWith (partials nyq amp) ps c : go p' cs
   in go 0 (g env)

-- | Сумма гармоник для одного сэмпла.
partials :: Double -> (Int -> Double) -> Double -> Double -> Double
partials nyq amp p f = go 0 1
  where
    f' = abs f
    kmax
      | f' <= 0 = 0
      | otherwise = min maxPartials (floor (nyq / f'))
    go !acc k
      | k > kmax = acc
      | a == 0 = go acc (k + 1)
      | otherwise = go (acc + a * gain * sin (fromIntegral k * p)) (k + 1)
      where
        a = amp k
        gain = partialGain (fromIntegral k * f' / nyq)

-- | Пила: (2/pi) * sum (-1)^(k+1) * sin (k*phi) / k, то есть phi/pi.
saw :: Sig -> Sig
saw = additive sawAmp

sawAmp :: Int -> Double
sawAmp k = (if odd k then 2 else -2) / pi / fromIntegral k

-- | Прямоугольник с управляемой шириной: width это доля периода наверху,
-- 0.5 даёт меандр, 0 и 1 дают тишину.
--
-- Считается как разность двух пил, сдвинутых по фазе на ширину. У такой
-- разности постоянная составляющая сокращается сама, поэтому модуляция
-- ширины не даёт скачков уровня; прямая сумма гармоник импульса дала бы
-- составляющую 2*width-1, которая ездила бы вместе с шириной. Уровни при
-- любой ширине -1 и 1.
--
-- Фаза считается один раз на обе пилы, поэтому частота вычисляется тоже
-- один раз.
pulse :: Sig -> Sig -> Sig
pulse width freq = Sig $ \env ->
  let k = twoPi / envRate env
      nyq = envRate env / 2
      go _ [] = []
      go !p ((ws, fs) : rest) = out : go p' rest
        where
          (ps, p') = phaseChunk k p fs
          out =
            U.izipWith
              (\i ph f -> pulsePartials nyq ph (twoPi * U.unsafeIndex ws i) f)
              ps
              fs
   in rechunk (blockOf env) (go 0 (align2 (runSig width env) (runSig freq env)))

-- | Сумма гармоник разности двух пил, сдвинутых на shift радиан.
pulsePartials :: Double -> Double -> Double -> Double -> Double
pulsePartials nyq p shift f = go 0 1
  where
    f' = abs f
    kmax
      | f' <= 0 = 0
      | otherwise = min maxPartials (floor (nyq / f'))
    go !acc k
      | k > kmax = acc
      | otherwise = go (acc + a * gain * (sin (kf * p) - sin (kf * (p + shift)))) (k + 1)
      where
        kf = fromIntegral k
        a = sawAmp k
        gain = partialGain (kf * f' / nyq)

-- | Меандр: (4/pi) * sum по нечётным sin (k*phi) / k.
square :: Sig -> Sig
square = additive $ \k ->
  if even k then 0 else 4 / pi / fromIntegral k

-- | Треугольник: (8/pi^2) * sum по нечётным (-1)^((k-1)/2) * sin (k*phi) / k^2.
tri :: Sig -> Sig
tri = additive $ \k ->
  if even k
    then 0
    else sign k * 8 / (pi * pi) / fromIntegral (k * k)
  where
    sign k = if even ((k - 1) `div` 2) then 1 else -1

-- | Белый шум в [-1, 1), детерминированный по seed и envSeed. Индексируется
-- номером сэмпла, поэтому от размера блока не зависит.
noise :: Int -> Sig
noise seed = Sig $ \env ->
  let s = envSeed env + seed
      b = blockOf env
      chunk j = U.generate b $ \i -> 2 * doubleAt s (j * b + i) - 1
   in map chunk [0 ..]
