-- | Стерео слоем поверх моно.
--
-- Ядро осталось моно: @Sig@ по разд. 3 это поток блоков одного канала, и
-- ломать его ради двух каналов незачем. Стерео это пара таких сигналов,
-- панорама и запись в два канала.
module Sound.Sig.Stereo
  ( Stereo (..)
  , mono
  , pan
  , bothChannels
  , mixStereo
  ) where

import Sound.Sig.Core

data Stereo = Stereo
  { leftChan :: Sig
  , rightChan :: Sig
  }

-- | По центру.
mono :: Sig -> Stereo
mono = pan 0

-- | Панорама по закону равной мощности: -1 слева, 0 по центру, 1 справа.
--
-- По центру каждый канал получает 1\/sqrt 2, поэтому мощность не зависит от
-- положения, а сумма каналов по центру громче одиночного канала в sqrt 2.
-- Отсюда и потеря в моно: центр в сумме поднимается в sqrt 2, а края лишь в
-- 1.075, то есть разведённое по краям в моно тише на четверть. Усиления
-- положительные, так что вычитания не бывает. NaN, как и у 'clampCut',
-- прижимается к нижнему краю, то есть влево.
--
-- @s@ входит в определение дважды, но share здесь намеренно нет: канал это
-- сигнал длиной в трек, и share держал бы его целиком в памяти (разд. 3).
-- Каналы читаются поблочно и в шаге, поэтому лишний счёт дешевле памяти.
pan :: Double -> Sig -> Stereo
pan p s = Stereo (s * constant (cos angle)) (s * constant (sin angle))
  where
    angle = (max (-1) (min 1 p) + 1) * pi / 4

-- | Обработать оба канала одним и тем же Fx.
bothChannels :: Fx -> Stereo -> Stereo
bothChannels fx (Stereo l r) = Stereo (fx l) (fx r)

-- | Сложить стерео-сигналы.
--
-- Без нейтрального элемента: литеральный 0 бесконечен и растянул бы микс.
mixStereo :: [Stereo] -> Stereo
mixStereo [] = Stereo (fromSamples []) (fromSamples [])
mixStereo (x : xs) = foldl add x xs
  where
    add (Stereo l r) (Stereo l' r') = Stereo (l + l') (r + r')
