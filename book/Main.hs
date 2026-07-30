-- | Рендер примеров книги в out\/book.
--
-- Каждая глава отдаёт список примеров, они пишутся файлами и слушаются
-- подряд. Кэша и стемов тут нет: пример это один сигнал.
module Main (main) where

import Book.Ch01 qualified as Ch01
import Book.Ch02 qualified as Ch02
import Book.Ch03 qualified as Ch03
import Book.Ch04 qualified as Ch04
import Book.Ch05 qualified as Ch05
import Book.Ch06 qualified as Ch06
import Book.Ch07 qualified as Ch07
import Book.Ch08 qualified as Ch08
import Book.Ch10 qualified as Ch10
import Book.Ch11 qualified as Ch11
import Book.Ch12 qualified as Ch12
import Book.Ch13 qualified as Ch13
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
        , Ch05.examples
        , Ch06.examples
        , Ch07.examples
        , Ch08.examples
        , Ch10.examples
        , Ch11.examples
        , Ch12.examples
        , Ch13.examples
        ]
    )
