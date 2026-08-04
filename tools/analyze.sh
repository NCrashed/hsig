#!/usr/bin/env bash
# Разбирает запись в величинах нашей оснастки: полосы, форма, шумность
# баса, сетка атак, ладовое содержание.
#
#   tools/analyze.sh "Sifting Through The Years.mp3"
#
# Декодирование чужая задача, поэтому его делает ffmpeg, а разбор наш.
# Промежуточный сырой поток кладётся в out и не удаляется: повторный разбор
# с другими параметрами не должен ждать декодера.
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "укажите аудиофайл" >&2
  exit 1
fi

src=$1
name=$(basename "${src%.*}")
raw="out/$name.f32"

mkdir -p out
if [ ! -f "$raw" ] || [ "$src" -nt "$raw" ]; then
  # Моно 44.1 кГц: разбору стерео не нужно, а верхняя полоса нужна целиком.
  "${FFMPEG:-ffmpeg}" -v error -i "$src" -ac 1 -ar 44100 -f f32le -y "$raw"
fi

cabal run -v0 analyze -- "$raw"
