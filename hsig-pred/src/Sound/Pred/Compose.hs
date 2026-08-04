-- | Композитор: жадный выбор такта по падению ошибки модели у слушателя.
--
-- Форма не постулируется, а должна возникнуть. Каждый такт выбирается из
-- нескольких равно законных продолжений истинной машины тот, который
-- сильнее прочих сокращает расхождение между тем, что слушатель думает, и
-- тем, что процесс делает на самом деле. Это оптимальное планирование
-- эксперимента, только учеником назначено ухо.
--
-- Про антипаттерн замкнутого контура (docs/PRED.md, разд. 9): настоящий
-- слушатель видит только выбранный такт. Кандидаты примеряются на его
-- копиях, которые тут же выбрасываются. Это модель ученика в голове
-- учителя, а не подглядывание.
module Sound.Pred.Compose
  ( BarOpts (..)
  , defaultBarOpts
  , Bar (..)
  , compose
  , modelError
  , probesOf
  , runMachine
  ) where

import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Sound.Pred.Dist
import Sound.Pred.Listener
import Sound.Pred.Machine
import Sound.Pred.Model (uniformsFrom)
import Sound.Pred.Orbifold

-- | Настройки поиска. Все три ограничения жёсткие: кандидат вне окна
-- отбрасывается, а не штрафуется.
data BarOpts = BarOpts
  { barLen :: Int
  -- ^ событий в такте
  , barCands :: Int
  -- ^ сколько продолжений примерить
  , barVlMax :: Double
  -- ^ потолок скачка голосоведения внутри такта, полутонов
  , barTypical :: Maybe Double
  -- ^ потолок среднего сюрприза такта __под истинной машиной__, бит.
  -- Ограничивает «насколько редким бывает материал», не заглядывая в
  -- слушателя, поэтому обратной связи не создаёт.
  , barWindow :: Maybe (Double, Double)
  -- ^ необязательное окно сюрприза __под моделью слушателя__: нижняя
  -- граница в битах и допустимое превышение над его ожиданием. По
  -- умолчанию 'Nothing', и это не лень, а измеренный результат: см.
  -- 'defaultBarOpts'.
  , barProbes :: Int
  -- ^ сколько пробных контекстов меряют ошибку модели
  , barOrder :: Int
  -- ^ порядок слушателя
  , barSeed :: Int
  }

-- | Восемь событий на такт, окно сюрприза выключено.
--
-- Выключено по измерению, а не по вкусу. Ошибка модели у слушателя за 12
-- тактов на кольцевой машине из шести состояний:
--
-- > с окном      1.22 -> 3.25   (втрое хуже)
-- > без окна     1.22 -> 0.33
-- > без выбора   1.22 -> 0.93   (контроль, честная выборка)
--
-- Окно задавалось через сюрприз, то есть через правдоподобие кандидата под
-- текущей моделью слушателя. Отбор по такому признаку оставляет типичное
-- под уже выученным: слушатель получает подтверждение вместо поправки и
-- уезжает от истины, оставаясь внутренне согласованным.
--
-- Правило общее: ограничение на отбор допустимо, только если оно не
-- зависит от правдоподобия под моделью слушателя. Голосоведение и
-- 'barTypical' таковы, окно сюрприза - нет. Разбор в docs/PRED.md, разд. 7.
--
-- 'barTypical' по умолчанию полтора бита, и на замеренной машине этот
-- порог __не связывает__: кривые с ним и без него совпадают до сотых,
-- средний сюрприз выбранных тактов под истинной машиной не превышает 1.29
-- при её @h_mu = 0.75@.
--
-- Это сам по себе результат. Ожидалось, что жадный поиск уйдёт в редкие
-- события, раз они информативнее всего. Он туда не уходит: выбранные такты
-- неожиданны для слушателя (2.4-4.0 бита), оставаясь типичными для процесса.
-- То есть педагогическая выборка здесь не смещает поток, а лишь переставляет
-- порядок подачи. Порог оставлен как перила для машин с длинным хвостом.
defaultBarOpts :: BarOpts
defaultBarOpts =
  BarOpts
    { barLen = 8
    , barCands = 24
    , barVlMax = 5
    , barTypical = Just 1.5
    , barWindow = Nothing
    , barProbes = 64
    , barOrder = 3
    , barSeed = 1
    }

-- | Выбранный такт и всё, по чему его выбрали.
--
-- Диагностика лежит рядом с материалом намеренно: без неё нельзя проверить,
-- возникла ли форма, а можно только заявить, что возникла.
data Bar s a = Bar
  { barSyms :: [a]
  , barStates :: [s]
  -- ^ причинное состояние перед каждым событием
  , barError :: Double
  -- ^ ошибка модели у слушателя перед этим тактом, бит
  , barGain :: Double
  -- ^ падение ошибки модели у слушателя за такт, бит
  , barSurprisal :: Double
  -- ^ средний сюрприз событий такта под моделью слушателя, бит
  , barExpected :: Double
  -- ^ средняя энтропия предсказаний слушателя на этом такте, бит
  , barTrueSurp :: Double
  -- ^ средний сюрприз событий такта под истинной машиной, бит
  , barLeap :: Double
  -- ^ наибольший скачок голосоведения, полутонов
  , barFeasible :: Bool
  -- ^ нашёлся ли кандидат внутри всех ограничений
  }

-- | Бесконечный след машины: состояние перед событием и само событие.
runMachine :: Machine s a -> Int -> [(s, a)]
runMachine m seed = go (machineStart m) (uniformsFrom seed)
  where
    go s (u : us) = (s, x) : go (machineStep m s x) us
      where
        x = sampleWith u (machineOut m s)
    go _ [] = []

-- | Пробные точки: история заданной длины и истинное состояние после неё.
--
-- Состояние берётся у машины, а не выводится из истории: иначе ошибка
-- модели мерялась бы относительно ещё одной оценки, а не относительно
-- истины.
probesOf :: Machine s a -> Int -> Int -> Int -> [([a], s)]
probesOf m order n seed =
  [ (map snd (take order (drop i path)), fst (path !! (i + order)))
  | i <- [0 .. n - 1]
  ]
  where
    path = take (n + order + 1) (runMachine m seed)

-- | Ошибка модели: среднее расхождение предсказаний слушателя с истинными
-- по пробным точкам, в битах.
modelError :: (Ord a) => Machine s a -> Listener a -> [([a], s)] -> Double
modelError m lis ps
  | null ps = 0
  | otherwise = sum [kl (machineOut m s) (predictAfter lis (reverse h)) | (h, s) <- ps] / fromIntegral (length ps)

-- | Сочинить заданное число тактов.
compose :: (Ord a) => BarOpts -> Machine s a -> (s -> Chord) -> Scale -> [a] -> Int -> [Bar s a]
compose opts m chordOf sc alphabet total
  | barLen opts < 1 = error "hsig-pred: такт короче одного события"
  | otherwise = go total 0 (machineStart m) (newListener (barOrder opts) alphabet) Nothing
  where
    ps = probesOf m (barOrder opts) (barProbes opts) (barSeed opts + 9973)

    go 0 _ _ _ _ = []
    -- Следующему такту передаётся последний прозвучавший аккорд, а не
    -- аккорд конечного состояния: конечное состояние прозвучит уже в
    -- следующем такте, и стык обязан считаться по слышимому.
    go n i st lis prev = bar : go (n - 1) (i + 1) end lis' (Just (chordOf (lastState sts)))
      where
        errBefore = modelError m lis ps
        cands = [sampleBar st (barSeed opts + i * 7919 + j) | j <- [0 .. barCands opts - 1]]
        scored = map (score lis errBefore prev) cands
        feasible = filter scoreOk scored
        picked
          | not (null feasible) = maximumBy (comparing scoreGain) feasible
          -- Ни один кандидат не влез в ограничения: берём наименее
          -- нарушающий, а не первый попавшийся, и честно помечаем такт.
          | otherwise = minimumBy (comparing violation) scored
        Scored g surp expd tsurp leap (syms, sts, end) ok = picked
        bar = Bar syms sts errBefore g surp expd tsurp leap ok
        lis' = trainOn lis syms

    -- barLen >= 1 проверено выше, поэтому список состояний такта непуст.
    lastState [] = error "hsig-pred: такт без состояний"
    lastState xs = last xs

    sampleBar st seed = walkBar (barLen opts) st (uniformsFrom seed) [] []
    walkBar 0 s _ syms sts = (reverse syms, reverse sts, s)
    walkBar k s (u : us) syms sts = walkBar (k - 1) (machineStep m s x) us (x : syms) (s : sts)
      where
        x = sampleWith u (machineOut m s)
    walkBar _ s [] syms sts = (reverse syms, reverse sts, s)

    score lis errBefore prev cand@(syms, sts, _) = Scored g surp expd tsurp leap cand ok
      where
        g = errBefore - modelError m (trainOn lis syms) ps
        surp = mean (onlineSurprisals lis syms)
        expd = mean (onlineEntropies lis syms)
        -- Сюрприз под истинной машиной: считается по её же состояниям,
        -- поэтому от слушателя не зависит вообще.
        tsurp = mean [surprisalOf (machineOut m s) x | (s, x) <- zip sts syms]
        cs = map chordOf sts
        line = maybe cs (: cs) prev
        leap = maximum (0 : zipWith (vlDist sc) line (drop 1 line))
        ok = inWindow surp expd && isTypical tsurp && leap <= barVlMax opts

    inWindow surp expd = case barWindow opts of
      Nothing -> True
      Just (lo, excess) -> surp >= lo && surp - expd <= excess

    isTypical tsurp = maybe True (tsurp <=) (barTypical opts)

    violation s = windowMiss + rare + over
      where
        over = max 0 (scoredLeap s - barVlMax opts)
        rare = maybe 0 (\t -> max 0 (scoredTrue s - t)) (barTypical opts)
        windowMiss = case barWindow opts of
          Nothing -> 0
          Just (lo, excess) ->
            max 0 (lo - scoredSurp s) + max 0 (scoredSurp s - scoredExp s - excess)

    mean xs = if null xs then 0 else sum xs / fromIntegral (length xs)

-- | Оценённый кандидат. Именованные поля, а не кортеж на шесть позиций:
-- ошибиться местом при таком количестве однотипных Double слишком легко.
data Scored s a = Scored
  { scoreGain :: Double
  , scoredSurp :: Double
  , scoredExp :: Double
  , scoredTrue :: Double
  , scoredLeap :: Double
  , _scoredCand :: ([a], [s], s)
  , scoreOk :: Bool
  }
