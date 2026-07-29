-- | Осцилляторы.
--
-- Пока фазовый интегратор и синус: единственная волна без гармоник, поэтому
-- в полосе по определению. Аддитивные saw/square/tri приходят на M1.
module Sound.Sig.Osc
  ( phase
  , sine
  ) where

import Data.Vector.Unboxed qualified as U
import Sound.Sig.Core

twoPi :: Double
twoPi = 2 * pi

-- | Приведение к [0, 2*pi): без него Double теряет точность на длинных нотах.
wrapPhase :: Double -> Double
wrapPhase p = p - twoPi * fromIntegral (floor (p / twoPi) :: Int)

-- | Частота в накопленную фазу: p[n] = p[n-1] + 2*pi*f[n]/sr, старт с нуля.
phase :: Sig -> Sig
phase (Sig g) = Sig $ \env ->
  let k = twoPi / envRate env
      step p f = wrapPhase (p + k * f)
      go _ [] = []
      go !p (c : cs) =
        -- scanl' даёт n+1 значений: n фаз блока и перенос на следующий.
        let acc = U.scanl' step p c
         in U.init acc : go (U.last acc) cs
   in go 0 (g env)

-- | Синус заданной частоты.
sine :: Sig -> Sig
sine = mapSig sin . phase
