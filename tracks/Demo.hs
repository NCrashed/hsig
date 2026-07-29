-- | Демо-трек: подряд всё, что умеет синтезатор на текущем этапе.
module Demo (main) where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.List (intercalate)
import Lead (leadWide)
import Sound.Sig

env :: Env
env = defaultEnv

rate :: Double
rate = envRate env

-- | Нота: длительность задаёт огибающая, потому что (*) обрезает по
-- короткому.
voice :: (Sig -> Sig) -> Double -> Double -> Sig
voice wave freq dur = wave (constant freq) * adsr 0.01 0.15 0.6 0.08 dur * 0.4

-- | Щипок: мгновенная атака и экспоненциальный спад. Хвост уже неслышим,
-- когда трапеция закрывает ноту.
pluck :: Double -> Double -> Sig
pluck freq dur = saw (constant freq) * expdecay 0.15 * trapezoid dur * 0.5

-- | Прямоугольник с краями по 10 мс, чтобы не щёлкало на стыках.
trapezoid :: Double -> Sig
trapezoid dur = line [(0, 0), (edge, 1), (dur - edge, 1), (dur, 0)]
  where
    edge = 0.01

-- | Пила через лестничный фильтр: катофф едет сверху вниз, резонанс поёт.
filtered :: Double -> Sig
filtered dur = ladder cut 0.85 (saw 110 * 0.3) * trapezoid dur
  where
    cut = line [(0, 8000), (dur, 200)]

-- | Почти цепочка из разд. 9: фильтр с резонансом и шейпер внутри
-- oversample, следом гребёнка. Катофф тянется дольше ноты, чтобы срез
-- задержки в oversample не обрезал хвост огибающей.
gritty :: Double -> Sig
gritty dur =
  saw 110
    * 0.3
    & oversample 8 (ladder cut 0.8 >>> shaper 4)
    & comb 0.011 0.6
    & (* (trapezoid dur * 0.3))
  where
    cut = line [(0, 6000), (dur + 0.1, 300)]

-- | Свип 20 - 8000 Гц: на нём слышно, что гармоники не заворачиваются.
sweep :: Double -> Sig
sweep dur = saw (fromSamples freqs) * trapezoid dur * 0.4
  where
    n = round (dur * rate) :: Int
    freqs = [20 + (8000 - 20) * fromIntegral i / fromIntegral n | i <- [0 .. n - 1]]

-- | Склейка встык. Микшера и планировщика ещё нет, они на M6.
oneAfterAnother :: [Sig] -> Sig
oneAfterAnother = fromSamples . concatMap (samples env)

track :: Sig
track =
  oneAfterAnother
    [ voice sine 440 1
    , voice saw 110 1.5
    , voice square 110 1.5
    , voice tri 110 1.5
    , voice (const (noise 0)) 0 1
    , pluck 220 1.5
    , filtered 3
    , gritty 3
    , sweep 4
    ]

-- Ритм из паттернов ---------------------------------------------------------

-- | Такт это два цикла: при четырёх долях в цикле выходит 120 ударов в
-- минуту. Темп задаётся паттерном, отдельного поля под него нет.
barCycles :: Time
barCycles = 2

bar :: Pattern a -> Pattern a
bar = slow barCycles

-- | Бас: пила через лестничный фильтр с быстрой огибающей катоффа.
bassInst :: Instrument
bassInst n =
  ladder cut 0.7 (saw (constant (noteFreq n)) * 0.5)
    * adsr 0.005 0.1 0.5 0.06 (noteDur n * 0.9)
    * constant (noteAmp n * 0.6)
  where
    cut = 200 + 2500 * expdecay 0.07

-- | Бочка: синус с быстрым спадом по высоте.
kickInst :: Instrument
kickInst n =
  sine (constant (noteFreq n) * (1 + 6 * expdecay 0.02))
    * adsr 0.001 0.15 0 0.02 (min 0.2 (noteDur n))
    * constant (noteAmp n)

-- | Хэт: отфильтрованный шум с мгновенной атакой.
hatInst :: Instrument
hatInst n =
  highpass 6000 (noise 1)
    * adsr 0.001 0.04 0 0.01 (noteDur n * 0.4)
    * constant (noteAmp n * 0.25)

-- | Шестнадцать тактов по два цикла: 32 секунды при 120 ударах в минуту.
trackSec :: Double
trackSec = 32

-- | Параметры компрессора одним значением: порог, отношение, атака,
-- восстановление. Одно и то же значение идёт и в сигнал, и в спецификацию
-- стема, иначе при односторонней правке они разъезжаются и кэш отдаёт
-- старый звук.
data Comp = Comp Double Double Double Double

compWith :: Comp -> Fx
compWith (Comp t r a rl) = compress t r a rl

compGainOf :: Comp -> Sig -> Sig
compGainOf (Comp t r a rl) = compressGain t r a rl

showComp :: Comp -> String
showComp (Comp t r a rl) = intercalate "/" (map show [t, r, a, rl])

-- | Трапеция по краям обязательна: рендер режет по времени жёстко, и без
-- неё последняя нота обрывается на середине релиза, то есть щелчком.
--
-- Лид это приёмочный патч разд. 9, бас и хэт держат ритм под ним.
sixteenBars :: [Stem]
sixteenBars =
  [ stemOf "kick" kickSpec (kickSig * gate)
  , stemOf "bass" bassSpec (duck (compWith bassComp (play bassInst bassPat)) * gate)
  , (stemOf "hat" hatSpec (duck (play hatInst hatPat) * gate)) {stemPan = 0.35}
  , -- Лид стерео: стек унисона разведён по панораме, поэтому каналы пишутся
    -- двумя стемами и ставятся по краям. Считаются они параллельно, так что
    -- двойная цена ноты в стену времени не превращается.
    (stemOf "leadL" (leadSpec "L") (leadOut leadL)) {stemPan = -1}
  , (stemOf "leadR" (leadSpec "R") (leadOut leadR)) {stemPan = 1}
  ]
  where
    -- Общий уровень: после компрессии и сайдчейна сумма стемов упирается в
    -- полную шкалу, а мастер-лимитера у renderTrack нет. Панорама по закону
    -- равной мощности отдаёт каналу 1/sqrt 2, отсюда и запас против моно.
    level = 0.9 :: Double
    kickLevel = 0.8 :: Double
    leadLevel = 0.5 :: Double
    gate = trapezoid trackSec * constant level

    -- Ритм читается строкой: мини-нотация Tidal, частоты в герцах.
    kickNotes = "55*4"
    bassNotes = "55 55 73.42 55"
    hatNotes = "1*8"
    -- Пауза в третьей доле, чтобы фраза дышала.
    leadNotes = "440 523.25 ~ 659.25"
    hatDegrade = 0.25 :: Double
    duckDepth = 0.7 :: Double
    leadEvery = 4 :: Int
    -- Пороги выставлены по замеренным максимумам следящих огибающих (бас
    -- 0.064, сумма каналов лида 0.272): взятые на глаз оказались выше их, и
    -- компрессоры не срабатывали ни разу.
    bassComp = Comp 0.04 4 0.005 0.08
    leadComp = Comp 0.18 3 0.01 0.1

    -- Спецификация это ключ кэша (см. renderStem), поэтому собирается из тех
    -- же значений, что идут в сигнал: всё, что меняет звук и не дошло до
    -- спецификации, досталось бы из кэша старым звуком.
    tempo = ", такт " <> show barCycles <> " цикла, уровень " <> show level
    kickSpec = "sine 55 pitchdrop x" <> show kickLevel <> ", " <> kickNotes <> tempo
    -- Накачка зависит от бочки, поэтому спецификация бочки входит в
    -- спецификации всех стемов, которые она качает: правка кика обязана
    -- перерендерить и их. Темп и общий уровень дописываются и своим краем:
    -- через накачку они пришли бы только транзитивно и исчезли бы вместе с
    -- ней.
    duckSpec = ", duck " <> show duckDepth <> " <" <> kickSpec <> ">" <> tempo
    bassSpec = "saw+ladder 0.7, " <> bassNotes <> ", comp " <> showComp bassComp <> duckSpec
    hatSpec = "noise+hp 6k, " <> hatNotes <> ", degrade " <> show hatDegrade <> duckSpec
    leadSpec side =
      "разд. 9 широкий "
        <> side
        <> ", "
        <> leadNotes
        <> ", every "
        <> show leadEvery
        <> " rev, comp "
        <> showComp leadComp
        <> " связанный x"
        <> show leadLevel
        <> duckSpec

    -- Бочка входит в определение пять раз, поэтому share (разд. 3). Это
    -- осознанное отклонение от оговорки в хаддоке share: сигнал длиной в
    -- трек держится в памяти целиком (12 МБ на 32 секундах), зато не
    -- считается впятеро.
    kickSig = share (play kickInst kickPat * constant kickLevel)
    duck = sidechain kickSig duckDepth
    kickPat = bar (noteOf <$> numbers kickNotes)
    bassPat = bar (noteOf <$> numbers bassNotes)
    hatPat = bar (degradeBy hatDegrade (noteOf <$> numbers hatNotes))
    leadPat = bar (every leadEvery rev (noteOf <$> numbers leadNotes))

    -- Компрессор на канал разваливал бы образ: тот канал, который сейчас
    -- громче, давился бы сильнее. Поэтому коэффициент считается один раз по
    -- сумме каналов и умножается на оба. Каналы под share: их читают и
    -- сумма, и выход, а оба стема считаются в одном процессе и делят кэш,
    -- иначе цепочка лида посчиталась бы не два раза, а шесть.
    leadStereo = playStereo leadWide leadPat
    leadL = share (leftChan leadStereo)
    leadR = share (rightChan leadStereo)
    leadGain = compGainOf leadComp (leadL + leadR)
    leadOut chan = duck (chan * leadGain) * gate * constant leadLevel

main :: IO ()
main = do
  report <- writeWav env Bits16 tour track
  putStrLn (tour <> ": пик " <> show (clipPeak report))
  trackPath <- renderTrack env trackSec "out/track.wav" sixteenBars
  putStrLn (trackPath <> ": 16 тактов, лид по разд. 9")
  where
    tour = "out/demo.wav"
