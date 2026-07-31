# hsig

[![CI](https://github.com/NCrashed/hsig/actions/workflows/ci.yml/badge.svg)](https://github.com/NCrashed/hsig/actions/workflows/ci.yml)

Оффлайн-синтезатор и DSL для электронной музыки на чистом Haskell. Трек это
обычный хаскелльный код, результат это WAV.

> **A note in English.** Everything here - documentation, code comments, the
> book - is in Russian. This is a personal project, and creative work is more
> fun in my native language, so I made no attempt to write it bilingually.
> The code itself is ordinary Haskell and the API names are English, so it is
> usable without Russian; the reasoning behind every decision, however, is
> not. Feel free to read, fork and translate. No support is promised.

## Что это

Библиотека для синтеза музыки без реального времени: считаем медленно, зато
честно. Осцилляторы [аддитивные](docs/book/glossary.md#additive), фильтры на
[TPT/ZDF](docs/book/glossary.md#tpt), ресемплинг на
[FIR с окном Кайзера](docs/book/glossary.md#fir). Приоритеты в таком порядке:
качество звука, эргономика комбинаторов, скорость рендера (последняя - по
остаточному принципу).

Незнакомые термины объясняет [глоссарий](docs/book/glossary.md), он же
собирает ссылки на подробные описания.

Целевой звук - плотное сатурированное электро в духе Vitalic, но библиотека
общая: в книге есть техно, эмбиент и драм-н-бэйс на тех же кубиках.

Что уже работает:

- сигнал как ленивый поток блоков, `Num`-инстансы с двумя семантиками длин;
- аддитивные `saw`/`square`/`tri`/`pulse`, детерминированный шум;
- огибающие `line`/`adsr`/`expdecay`/`gate`;
- фильтры `onepole`/`highpass`/`ladder`/`svf` и его режимы;
- нелинейности `shaper`/`cubicnl`/`clip`/`decimate`, `oversample` и
  произвольный `resample`;
- задержки `delay`/`vdelay`/`comb`/`allpass`, динамика `compress`/`sidechain`;
- партитура с семантикой Tidal и мини-нотацией строками (`"bd*4"`, `"a1 ~ c2"`);
- стерео: панорама, `orbit` (параметрическое вращение), бинауральная свёртка
  на измеренных HRTF (KEMAR), стерео-инструменты;
- стемы с кэшем, параллельный рендер, сведение и мастер-обработка;
- запись WAV 16/24 бита и float32 с дизерингом и отчётом о клиппинге.

## Быстрый старт

Нужен [Nix](https://nixos.org/download/) с включёнными flakes. Всё
остальное приедет само.

```
git clone https://github.com/NCrashed/hsig
cd hsig
nix develop            # шелл с GHC 9.10, cabal, HLS, hlint, fourmolu
cabal build all
cabal test             # 583 теста
cabal run demo         # out/track.wav: демо-трек на 16 тактов
cabal run book         # out/book/*.wav: все примеры книги
cabal run minimal      # out/minimal.wav: самый маленький трек
```

Первый `nix develop` собирает окружение и занимает несколько минут; дальше
всё из кэша. `cabal run demo` считает около минуты, повторный прогон - около
двадцати секунд, потому что дорогие стемы берутся из кэша.

Без Nix тоже можно: GHC 9.10 и cabal, а из системных зависимостей нужен
только FFTW (для тестов). Единственное, что придётся достать руками, -
набор HRTF: [KEMAR compact](https://sound.media.mit.edu/resources/KEMAR.html),
распаковать и указать путь в `HSIG_HRTF`. В дев-шелле он приезжает сам.

## Куда смотреть дальше

- **[Книга](docs/book/README.md)** - как этим пользоваться, от первого синуса
  до готового трека, с примерами, которые можно послушать. Часть I - основы
  (сигнал, осцилляторы, огибающие, фильтры, насыщение, партитура, динамика,
  стерео, сборка трека), часть II - рецепты жанров. Термины объясняются в
  [глоссарии](docs/book/glossary.md).
- **[docs/DESIGN.md](docs/DESIGN.md)** - зачем всё устроено именно так:
  ключевые решения, требования к DSP, дорожная карта, приёмочные проверки.
  Читать, если собираетесь править библиотеку, а не пользоваться ей.
- **[tracks/Demo.hs](tracks/Demo.hs)** - настоящий трек целиком, 134 строки.
  Самый быстрый способ начать свой: скопировать и заменить патчи с
  паттернами.
- **[patches/](patches/)** - патчи: широкий унисонный лид и вращающийся
  реактор из трёх стержней.

## Свой трек

Проще всего - править `tracks/Demo.hs`. Если нужен отдельный файл, добавьте
исполняемый в `hsig.cabal` по образцу `demo` и не забудьте
`default-extensions: OverloadedStrings` (без него строки не станут
паттернами).

Минимальный трек целиком - это `tracks/Minimal.hs`, его можно запустить
(`cabal run minimal`) и переделать под себя:

```haskell file=tracks/Minimal.hs
-- | Самый маленький трек, который имеет смысл: одна партия, одно окно.
-- Отсюда удобно начинать свой, см. README.
module Minimal (main) where

import Sound.Sig

kick :: Instrument
kick n =
  sine (constant (noteFreq n) * (1 + 6 * expdecay 0.02))
    * adsr 0.001 0.15 0 0.02 0.2

track :: [Stem]
track = [stem "kick" (play kick (slow 2 "a1*4") * gate 0.01 8)]

main :: IO ()
main = renderTrack defaultEnv "out/minimal.wav" track >>= putStrLn
```

Три вещи, о которых спотыкаются в начале:

1. **Стем обязан кончаться сам.** Паттерн бесконечен, длина трека берётся из
   материала, поэтому окно (`gate`) или огибающая обязательны. Забытое окно
   ловится потолком в 600 секунд с объяснением.
2. **Складывать список сигналов надо через `mix`, а не `sum`.** У `sum`
   нейтральный элемент это литеральный `0`, то есть бесконечная константа.
3. **Сигнал, использованный дважды, оборачивается в `share`.** Иначе он
   считается дважды: `Sig` это функция, а не буфер.

## Разработка

```
cabal test                       # tasty, включая проверку примеров книги
hlint src test tracks patches book
fourmolu -i src test tracks patches book
nix develop .#llvm               # шелл с LLVM, если нужен -fllvm
nix flake check                  # то же, что гоняет CI: сборка и тесты
```

Тесты не только проверяют код, но и сверяют каждый показанный в книге
фрагмент с исходником: разошлись - тест падает и показывает обе версии. Они
же ловят ссылки в никуда - и на термины глоссария, и на звуковые примеры.

Звук примеров хранится сжатым (`docs/book/audio`, около 7 МБ), потому что wav
это результат сборки. После правки примеров:

```
cabal run book && tools/book-audio.sh
```

CI (`.github/workflows/ci.yml`) делает то же самое, что можно сделать руками:
`nix flake check` (сборка и 583 теста в песочнице), `hlint`, проверка
форматирования Haskell и Nix, рендер демо-трека с проверкой размера файла.
Готовый `out/track.wav` остаётся артефактом прогона, так что результат сборки
можно послушать, ничего не собирая.

## Лицензия

MIT, см. [LICENSE](LICENSE).
