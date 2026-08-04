#!/usr/bin/env bash
# Сжимает отрендеренные треки из out в mp3 рядом с исходником.
#
# Wav с трёхминутного трека весит 35 МБ: слушать с телефона, переслать или
# приложить к письму неудобно. Mp3 весит около трёх и на этом материале от
# исходника на слух не отличается.
#
#   cabal run keygen -- 48 && tools/mp3.sh keygen-48
#   tools/mp3.sh                     # все wav в out, включая стемы
#   tools/mp3.sh out/pred.wav        # можно и путём
#
# Скрипт отдельный, а не шаг рендера, по той же причине, что и
# tools/book-audio.sh: рендер не должен зависеть от стороннего кодировщика.
set -euo pipefail

if ! command -v lame >/dev/null; then
  echo "нет lame: войдите в дев-шелл (nix develop)" >&2
  exit 1
fi

# Аргумент это либо путь к wav, либо имя трека без каталога и расширения.
# Угадывать, какой из файлов «финальный», скрипт не пытается: имя называет
# вызывающий, иначе однажды сожмётся не то.
resolve() {
  case "$1" in
    *.wav) echo "$1" ;;
    */*) echo "$1.wav" ;;
    *) echo "out/$1.wav" ;;
  esac
}

if [ $# -eq 0 ]; then
  shopt -s nullglob
  files=(out/*.wav)
  if [ ${#files[@]} -eq 0 ]; then
    echo "в out нет wav: сначала отрендерьте трек" >&2
    exit 1
  fi
else
  files=()
  for arg in "$@"; do
    files+=("$(resolve "$arg")")
  done
fi

for wav in "${files[@]}"; do
  if [ ! -f "$wav" ]; then
    echo "нет файла: $wav" >&2
    exit 1
  fi
  mp3="${wav%.wav}.mp3"
  # Пропускаем уже сжатое: перегонять минуты звука на каждом прогоне незачем.
  if [ -f "$mp3" ] && [ "$mp3" -nt "$wav" ]; then
    echo "свежий:  $mp3"
    continue
  fi
  # -V2: переменный битрейт около 190 кбит/с. Выше, чем -V4 у примеров
  # книги, намеренно: там одиночные тембры, а тут полный микс с шумовыми
  # тарелками, на которых экономия слышна.
  lame --quiet -V2 "$wav" "$mp3"
  printf 'готово: %-28s %s -> %s\n' "$mp3" \
    "$(du -h "$wav" | cut -f1)" "$(du -h "$mp3" | cut -f1)"
done
