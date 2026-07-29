-- | Динамическая обработка.
--
-- Пороги и глубины задаются в линейной амплитуде, а не в децибелах: во
-- всей библиотеке уровни линейные, и смешивать две шкалы дороже, чем
-- написать 0.2 вместо -14 дБ.
module Sound.Sig.Dynamics
  ( compress
  , sidechain
  ) where

import Data.Vector.Unboxed qualified as U
import Sound.Sig.Block (align2, sweep)
import Sound.Sig.Core

-- | Коэффициент однополюсного следящего звена по времени в секундах.
follow :: Double -> Double -> Double
follow secs rate
  | secs <= 0 = 1
  | otherwise = 1 - exp (negate (1 / (secs * rate)))

-- | Следящая огибающая по модулю сигнала: вверх с постоянной атаки, вниз с
-- постоянной восстановления.
track :: Double -> Double -> Double -> Double -> Double
track up down env x = env + (a - env) * (if a > env then up else down)
  where
    a = abs x

-- | Компрессор: порог, отношение, атака и восстановление в секундах.
--
-- Выше порога превышение делится на отношение, поэтому вход A даёт
-- @thresh + (A - thresh)\/ratio@ в установившемся режиме.
compress :: Double -> Double -> Double -> Double -> Fx
compress thresh ratio att rel input = Sig $ \env ->
  let up = follow att (envRate env)
      down = follow rel (envRate env)
      t = max 1e-9 thresh
      r = max 1e-9 ratio
      step e x = (x * gain, e')
        where
          e' = track up down e x
          gain
            | e' > t = (t + (e' - t) / r) / e'
            | otherwise = 1
      go _ [] = []
      go !e (c : cs) = out : go e' cs
        where
          (out, e') = sweep (\s i -> step s (U.unsafeIndex c i)) (U.length c) e
   in go 0 (runSig input env)

-- | Сайдчейн: управляющий сигнал давит вход на глубину 0..1.
--
-- Времена следящей огибающей фиксированы (быстрая атака, отпускание за
-- 120 мс): именно они дают привычную накачку, а выносить их в аргументы
-- разд. 5 не просит.
sidechain :: Sig -> Double -> Fx
sidechain ctrl depth input = Sig $ \env ->
  let up = follow 0.002 (envRate env)
      down = follow 0.12 (envRate env)
      d = max 0 (min 1 depth)
      go _ [] = []
      go !e ((c, x) : rest) = out : go e' rest
        where
          step s i =
            let s' = track up down s (U.unsafeIndex c i)
             in (U.unsafeIndex x i * (1 - d * min 1 s'), s')
          (out, e') = sweep step (U.length x) e
   in rechunk (blockOf env) (go 0 (align2 (runSig ctrl env) (runSig input env)))
