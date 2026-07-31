# Глава 18. Лады, аккорды, арпеджио

До сих пор ноты писались именами: `"a3 c4 e4"`. Это честно, но неудобно, как
только появляется гармония: перенести фразу в другую тональность значит
переписать каждую ноту, а собрать аккорд - выписать три строки в `stack`.
Здесь то же самое короче.

## Ступени вместо нот

`scale лад основа` переводит номера ступеней в частоты:

```haskell file=book/Book/Ch18.hs sym=runUp
runUp :: Sig
runUp = play rhodes (slow 2 (scale "minor" "a3" "0 1 2 3 4 5 6 7")) * gate 0.01 8
```

[`18-degrees.mp3`](audio/18-degrees.mp3) - восемь ступеней минора от ля малой
октавы. Ступень 7 это уже октава: номера за пределами лада переносятся
автоматически, в обе стороны. Отрицательные идут вниз, поэтому `-1` это
седьмая ступень предыдущей октавы.

Главная выгода не в краткости, а в том, что мелодия становится числами и с
ней можно считать:

```haskell file=book/Book/Ch18.hs sym=transposed
transposed :: Sig
transposed = play rhodes (slow 2 (scale "minor" "a3" (every 2 (fmap (+ 2)) "0 2 4 2"))) * gate 0.01 8
```

[`18-transposed.mp3`](audio/18-transposed.mp3): каждый второй цикл фраза
уезжает на две ступени вверх. Не на два полутона - именно на две ступени, то
есть она остаётся в ладу и не фальшивит. Ровно за этим ступени и нужны.

Лады: `major`, `minor`, `harmonicMinor`, `melodicMinor`, все семь церковных
(`dorian`, `phrygian`, `lydian`, `mixolydian`, `locrian`), пентатоники
(`majPent`, `minPent`), `blues`, `wholetone`, `chromatic`. Таблица в
`Sound.Sig.Harmony`, имена как в Tidal.

## Аккорды в строке

Апостроф после ноты означает аккорд: `"c4'maj"` это три ноты в одном отрезке.

```haskell file=book/Book/Ch18.hs sym=chords
chords :: Sig
chords = play rhodes (slow 2 "a3'min c4'maj e3'min7 f3'maj7") * 0.45 * gate 0.01 16
```

[`18-chords.mp3`](audio/18-chords.mp3). Под капотом это `stack` из трёх-пяти
`pure`, поэтому аккорд ведёт себя как обычные одновременные ноты: его можно
прореживать, сдвигать, накладывать - всё, что умеют паттерны.

Набор: `maj`, `min`, `maj7`, `min7`, `dom7`, `dim`, `dim7`, `aug`, `sus2`,
`sus4`, `six`, `m6`, `add9`, `m9`, `maj9`, `nine`, `five`, `m7b5`.

## Арпеджио

`arp` разворачивает одновременные ноты в последовательность внутри их же
отрезка:

```haskell file=book/Book/Ch18.hs sym=arpUp
arpUp :: Sig
arpUp = play rhodes (arp "updown" (slow 2 "a3'min c4'maj e3'min7 f3'maj7")) * 0.7 * gate 0.01 16
```

[`18-arp.mp3`](audio/18-arp.mp3). Порядки: `up`, `down`, `updown` (вверх и
обратно без повтора краёв), `downup`, `thumbup` (нижняя нота через одну, как
играют левой рукой).

Важно, что `arp` работает не с аккордом как со специальным типом, а с любыми
одновременными событиями. Сложили две партии через `stack` - `arp` разложит и
их; это тот же приём, что и везде в этой алгебре.

## Всё вместе

```haskell file=book/Book/Ch18.hs sym=progression
progression :: Stereo
progression = bothChannels (\c -> shaper 1.2 (c * gate 0.05 16 * 0.45) * 0.9) mixed
  where
    harmony = slow 4 "a3'min f3'maj7 c4'maj e3'min7"
    keys = play rhodes (arp "up" (fast 2 harmony)) * 1.1
    pad = play rhodes harmony * 0.5
    bass = play warmBass (slow 4 (scale "minor" "a1" "0 ~ ~ ~ 5 ~ ~ ~ 2 ~ ~ ~ 4 ~ ~ ~")) * 1.2
    mixed = mixStereo [pan (-0.3) keys, pan 0.25 pad, mono bass]
```

[`18-progression.mp3`](audio/18-progression.mp3). Гармония написана один раз
и используется трижды: как арпеджио вдвое быстрее, как выдержанный пэд и как
опора для баса. Бас при этом идёт ступенями того же лада, а не нотами -
поэтому смена тональности здесь это правка одной строки.

Замер: пик 0.69, среднеквадратичное -14 dBFS.

## Что дальше

Панорама из главы 8 не умеет отличать перед от зада. Глава 19 про то, как
это чинится измеренными откликами головы.
