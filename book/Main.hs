-- | Рендер примеров книги в out\/book.
--
-- Каждая глава отдаёт список примеров, они пишутся файлами и слушаются
-- подряд. Кэша и стемов тут нет: пример это один сигнал.
module Main (main) where

import Book.Ch01 qualified as Ch01
import Book.Ch02 qualified as Ch02
import Book.Ch03 qualified as Ch03
import Book.Ch04 qualified as Ch04
import Book.Prelude (renderExamples)

main :: IO ()
main =
  renderExamples
    "out/book"
    ( concat
        [ Ch01.examples
        , Ch02.examples
        , Ch03.examples
        , Ch04.examples
        ]
    )
