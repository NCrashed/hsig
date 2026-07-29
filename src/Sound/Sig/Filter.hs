-- | Фильтры.
--
-- Топология TPT (Zavalishin, zero-delay feedback). Рекурсия по сэмплам
-- живёт здесь, внутри runST, наружу торчит только Fx (разд. 2 дизайна).
module Sound.Sig.Filter
  ( onepole
  , highpass
  , ladder
  ) where

import Data.Vector.Unboxed qualified as U
import Sound.Sig.Block (align2, align3, sweep)
import Sound.Sig.Core

-- Однополюсник ------------------------------------------------------------

-- | TPT-однополюсник, аргумент это частота среза.
onepole :: Sig -> Fx
onepole = filter1 0 step
  where
    step env s fc = pole (gainOf env fc) s

-- | Дополнение однополюсника до единицы, то есть ФВЧ первого порядка.
--
-- Основное применение это блокиратор постоянной составляющей после
-- асимметричных шейперов: срез в единицы герц убирает DC, не трогая звук.
-- Вход используется дважды, поэтому share: правило разд. 3.
highpass :: Sig -> Fx
highpass cutoff x = zipChunks Truncate (-) sx (onepole cutoff sx)
  where
    sx = share x

-- | Одно TPT-звено: выход и новое состояние.
--
-- @v = (x - s)*G;  y = v + s;  s' = y + v@
pole :: Double -> Double -> Double -> (Double, Double)
pole bigG s x = (y, y + v)
  where
    v = (x - s) * bigG
    y = v + s

-- | Мгновенный коэффициент звена: @g = tan (pi*fc/sr)@, @G = g/(1+g)@.
gainOf :: Env -> Double -> Double
gainOf env fc = g / (1 + g)
  where
    g = tan (pi * clampCut env fc / envRate env)

-- | Катофф зажимается в полосу: на Найквисте tan уходит в бесконечность, а
-- модулировать катофф можно чем угодно, включая огибающую с перелётом.
--
-- NaN на входе даёт нижнюю границу, то есть закрытый фильтр, а не NaN на
-- выходе: сравнение с NaN всегда ложно, поэтому max возвращает первый
-- аргумент. То же с резонансом: NaN даёт ноль.
clampCut :: Env -> Double -> Double
clampCut env fc = max 1e-4 (min (0.49 * envRate env) fc)

-- Лестничный фильтр -------------------------------------------------------

-- | Состояние четырёх звеньев плюс выход прошлого сэмпла: он идёт
-- начальным приближением для итераций.
data Ladder = Ladder !Double !Double !Double !Double !Double

-- | Лестничный фильтр: катофф, резонанс 0..1.
--
-- Четыре звена подряд, обратная связь @k = 4*res@ с @tanh@ в петле: именно
-- она даёт компрессирующийся резонанс вместо цифрового визга. Компенсацию
-- падения громкости при росте резонанса не делаем, разд. 6.3 разрешает.
--
-- Граница метода: четыре итерации сходятся, пока @k*G^4@ мал. До катоффа
-- около @0.2*sr@ решение практически точное, выше фильтр самовозбуждается
-- раньше, чем предсказывает точное решение петли. Рабочие катоффы (патч
-- разд. 9 живёт ниже @0.12*sr@, а под oversample и того ниже) сюда не
-- попадают.
ladder :: Sig -> Sig -> Fx
ladder = filter2 (Ladder 0 0 0 0 0) step
  where
    step env st fc r = ladderStep (gainOf env fc) (4 * max 0 (min 1 r)) st

-- | Один сэмпл лестничного фильтра.
--
-- Петля нелинейная, поэтому решается итерациями по замороженному
-- состоянию: @u <- x - k*tanh (L u)@. Четыре шага, состояние коммитится
-- только после них (разд. 6.3). Итерация сжимающая, пока @k*G^4 < 1@; выше
-- она перестаёт сходиться, но tanh держит результат ограниченным.
ladderStep :: Double -> Double -> Ladder -> Double -> (Double, Ladder)
ladderStep bigG k (Ladder s1 s2 s3 s4 yPrev) x = (y, Ladder t1 t2 t3 t4 y)
  where
    frozen u = let (o, _, _, _, _) = chain bigG s1 s2 s3 s4 u in o
    next u = x - k * tanh (frozen u)
    u0 = x - k * tanh yPrev
    u4 = next (next (next (next u0)))
    (y, t1, t2, t3, t4) = chain bigG s1 s2 s3 s4 u4

-- | Цепочка из четырёх звеньев: выход и новые состояния.
chain
  :: Double
  -> Double
  -> Double
  -> Double
  -> Double
  -> Double
  -> (Double, Double, Double, Double, Double)
chain bigG s1 s2 s3 s4 u = (y4, t1, t2, t3, t4)
  where
    (y1, t1) = pole bigG s1 u
    (y2, t2) = pole bigG s2 y1
    (y3, t3) = pole bigG s3 y2
    (y4, t4) = pole bigG s4 y3

-- Обвязка -----------------------------------------------------------------

-- | Фильтр с одним управляющим сигналом.
filter1 :: s -> (Env -> s -> Double -> Double -> (Double, s)) -> Sig -> Fx
filter1 s0 step ctrl input = Sig $ \env ->
  let go _ [] = []
      go !s ((c, x) : rest) =
        let (out, s') = sweep (\st i -> step env st (U.unsafeIndex c i) (U.unsafeIndex x i)) (U.length x) s
         in out : go s' rest
   in rechunk (blockOf env) (go s0 (align2 (runSig ctrl env) (runSig input env)))

-- | Фильтр с двумя управляющими сигналами.
filter2 :: s -> (Env -> s -> Double -> Double -> Double -> (Double, s)) -> Sig -> Sig -> Fx
filter2 s0 step ctrl1 ctrl2 input = Sig $ \env ->
  let go _ [] = []
      go !s ((a, b, x) : rest) =
        let f st i = step env st (U.unsafeIndex a i) (U.unsafeIndex b i) (U.unsafeIndex x i)
            (out, s') = sweep f (U.length x) s
         in out : go s' rest
   in rechunk (blockOf env) (go s0 (align3 (runSig ctrl1 env) (runSig ctrl2 env) (runSig input env)))
