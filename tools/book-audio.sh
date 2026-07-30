#!/usr/bin/env bash
# Перекодирует примеры книги из out/book в docs/book/audio.
#
# Wav-файлы это результат сборки и в репозиторий не идут (23 МБ и заново при
# каждом прогоне), а читателю с GitHub нужен звук по ссылке. Поэтому в
# репозитории лежит сжатая копия, и её надо обновлять после правки примеров:
#
#   cabal run book && tools/book-audio.sh
set -euo pipefail

src=out/book
dst=docs/book/audio

if [ ! -d "$src" ]; then
  echo "нет $src: сначала cabal run book" >&2
  exit 1
fi

mkdir -p "$dst"
for wav in "$src"/*.wav; do
  name=$(basename "$wav" .wav)
  # -V4: переменный битрейт около 160 кбит/с, на слух неотличимо от исходника
  # на этом материале, а разница в весе десятикратная.
  lame --quiet -V4 "$wav" "$dst/$name.mp3"
done

# Файлы, которых больше нет в примерах, уносим за собой: иначе каталог
# зарастает, а ссылки в главах указывают на давно переименованное.
for mp3 in "$dst"/*.mp3; do
  name=$(basename "$mp3" .mp3)
  if [ ! -f "$src/$name.wav" ]; then
    rm "$mp3"
    echo "убрано: $name.mp3"
  fi
done

du -sh "$dst"
