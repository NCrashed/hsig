-- | Демо-трек: шестнадцать тактов на 120 ударах в минуту.
--
-- Написан так, как задаёт разд. 10 дизайна: имя стема плюс сигнал, ритм
-- строкой, ноты по именам. Кэш включён только там, где рендер долгий, и
-- версией в строке: правишь патч - двигаешь версию.
module Demo (main) where

import Control.Category ((>>>))
import Lead (leadWide)
import Reactor (reactor)
import Sound.Sig

-- | Такт это два цикла: при четырёх долях в цикле выходит 120 ударов в
-- минуту. Темп задаётся паттерном, отдельного поля под него нет.
bar :: Pattern a -> Pattern a
bar = slow 2

-- | Шестнадцать тактов по два цикла.
trackSec :: Double
trackSec = 32

-- | Окно всего трека: оно и задаёт длину, потому что паттерн бесконечен.
-- Края гасятся, иначе на обрыве щёлкнет.
window :: Sig
window = gate 0.01 trackSec

-- | Трим перед мастером: им задаётся, насколько плотно микс входит в
-- насыщение (см. master), поэтому он общий на все стемы.
mixLevel :: Sig
mixLevel = 0.9

-- Инструменты ---------------------------------------------------------------

-- | Бочка: синус с быстрым спадом по высоте.
kickInst :: Instrument
kickInst n =
  sine (constant (noteFreq n) * (1 + 6 * expdecay 0.02))
    * adsr 0.001 0.15 0 0.02 (min 0.2 (noteDur n))
    * constant (noteAmp n)

-- | Бас: пила через лестничный фильтр с быстрой огибающей катоффа.
bassInst :: Instrument
bassInst n =
  ladder cut 0.7 (saw (constant (noteFreq n)) * 0.5)
    * adsr 0.005 0.1 0.5 0.06 (noteDur n * 0.9)
    * constant (noteAmp n * 0.6)
  where
    cut = 200 + 2500 * expdecay 0.07

-- | Хэт: отфильтрованный шум с мгновенной атакой. Частота ему не нужна, он
-- живёт по метке из паттерна.
hatInst :: Instrument
hatInst n =
  highpass 6000 (noise 1)
    * adsr 0.001 0.04 0 0.01 (noteDur n * 0.4)
    * constant (noteAmp n * 0.25)

-- Трек ----------------------------------------------------------------------

-- | Баланс выставлен по замеру среднеквадратичных: бас, лид и реактор на
-- 6-9 дБ ниже бочки, хэт на 11. Уровни это Sig, поэтому в местах
-- использования не нужен constant.
kickLevel, bassLevel, hatLevel, leadLevel, reactorLevel :: Sig
kickLevel = 0.45
bassLevel = 2
hatLevel = 1.5
leadLevel = 1.2
reactorLevel = 2.2

-- | Шина стема: накачка от бочки, окно трека и уровень в миксе. Бочка идёт
-- мимо шины, качать её нечем.
bus :: Sig -> Fx
bus level sig = duck sig * window * level * mixLevel

track :: [Stem]
track =
  [ stem "kick" (kickSig * window * mixLevel)
  , stem "bass" (bus bassLevel (compress 0.04 4 0.005 0.08 (play bassInst (bar "a1 a1 d2 a1"))))
  , panned 0.35 (stem "hat" (bus hatLevel (play hatInst (bar (degradeBy 0.25 "hh*8")))))
  ]
    -- Кэш только на дорогих стемах: лид и реактор считаются десятки секунд.
    -- Версия в строке двигается вместе с правкой патча, длина входит в неё же
    -- (в ключ она не попадает, см. cached).
    <> map (cached (version "лид разд. 9, широкий, v1")) (stereoStems "lead" leadOut)
    <> map (cached (version "реактор, три стержня, v1")) (stereoStems "reactor" reactorOut)
  where
    version s = s <> ", " <> show trackSec <> " с"

-- | Бочка входит в определение пять раз, поэтому share (разд. 3): она же
-- управляет накачкой остальных стемов. Осознанное отклонение от хаддока
-- share: сигнал длиной в трек стоит памяти (около 12 МБ на 32 секундах),
-- зато не считается пятикратно.
kickSig :: Sig
kickSig = share (play kickInst (bar "a1*4") * kickLevel)

-- | Накачка от бочки.
duck :: Fx
duck = sidechain kickSig 0.7

-- | Лид: широкий унисон, каналы сжимаются одним коэффициентом по сумме,
-- иначе громче звучащий канал давился бы сильнее и образ дышал бы.
leadOut :: Stereo
leadOut = bothChannels out (Stereo l r)
  where
    Stereo wideL wideR = playStereo leadWide (bar (every 4 rev "a4 c5 ~ e5"))
    l = share wideL
    r = share wideR
    gain = compressGain 0.18 3 0.01 0.1 (l + r)
    out chan = bus leadLevel (chan * gain)

-- | Реактор: три пульсирующих стержня, обходящих голову. Оборот за такт,
-- вспышки восьмыми, разнос нулевой (см. Reactor).
reactorOut :: Stereo
reactorOut = bothChannels (bus reactorLevel) sig
  where
    -- Оборот за такт это 0.5 Гц, вспышки восьмыми это 4 Гц.
    sig = reactor 0.5 4 0 (map noteHz ["a4", "c5", "e5"])

-- | Мастер-каскад вместо лимитера.
--
-- Пиков выше 0.8 в миксе пара десятков на полтора миллиона сэмплов, и все на
-- совпадении атаки бочки с нотой лида. Резать ради них весь трек на 3 дБ
-- нельзя, а tanh поднимает середину и придерживает верхушки: нормировка у
-- shaper такая, что полная шкала остаётся на месте. Плотность от него растёт
-- на 3 дБ. Трим после насыщения оставляет запас.
--
-- Обработка идёт при сведении, поэтому её правка не сбивает кэш стемов.
master :: Stereo -> Stereo
master = bothChannels (shaper 1.2 >>> (* 0.9))

main :: IO ()
main = do
  path <- renderTrackWith defaultEnv "out/track.wav" master track
  putStrLn (path <> ": тактов " <> show (round (trackSec / 2) :: Int))
