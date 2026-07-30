-- | Книга: код в главах обязан совпадать с кодом, который собирается.
--
-- Проза живёт в docs\/book, код глав в book\/Book. Скопированный в Markdown
-- фрагмент расходится с библиотекой на первой же правке API (так уже было с
-- примером из разд. 10 дизайна), поэтому каждый блок помечен файлом и
-- символом, а тест сверяет его с исходником дословно.
module BookSpec (tests) where

import Control.Monad (filterM, forM)
import Data.List (isPrefixOf, isSuffixOf, sort, stripPrefix)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

-- | Путь относительно корня проекта: тесты запускаются оттуда. Файлы в
-- пометках блоков тоже пишутся от корня, поэтому глава может цитировать и
-- свой код, и код библиотеки.
bookDir :: FilePath
bookDir = "docs/book"

tests :: TestTree
tests =
  testGroup
    "Книга"
    [ testCase "блоки глав совпадают с кодом" $ do
        chapters <- chapterFiles
        assertBool "глав нет" (not (null chapters))
        -- README тоже показывает код, и протухает он так же.
        problems <- concat <$> mapM checkChapter ("README.md" : chapters)
        assertBool (unlines ("" : problems)) (null problems)
    , -- Помеченный блок это единственный способ показать код: без пометки он
      -- ничем не проверяется и живёт своей жизнью.
      testCase "каждый блок помечен файлом и символом" $ do
        chapters <- chapterFiles
        loose <- concat <$> mapM looseBlocks chapters
        assertBool (unlines ("непомеченные блоки haskell:" : loose)) (null loose)
    , -- Термины объясняются в одном месте, главы на него ссылаются. Ссылка в
      -- несуществующий якорь оставляет новичка без объяснения, а увидеть это
      -- глазами нельзя: якорь молча не срабатывает.
      testCase "ссылки в глоссарий ведут в существующие якоря" $ do
        chapters <- chapterFiles
        known <- idsOf <$> readFile (bookDir </> "glossary.md")
        used <- concat <$> mapM (fmap anchorsOf . readFile) chapters
        assertBool "главы не ссылаются на глоссарий" (not (null used))
        let missing = [a | a <- used, a `notElem` known]
        assertBool ("нет таких якорей: " <> unwords missing) (null missing)
    , -- Оглавление это вход в книгу: глава, на которую нет ссылки, не
      -- существует для читателя.
      testCase "оглавление и главы сходятся" $ do
        chapters <- map takeName <$> chapterFiles
        index <- readFile (bookDir </> "README.md")
        let linked = sort (linksOf index)
            present = sort chapters
        missing <- filterM (fmap not . doesFileExist . (bookDir </>)) linked
        assertBool ("ссылки в никуда: " <> unwords missing) (null missing)
        assertEqual "оглавление разошлось с главами" present linked
    ]

-- | Файлы глав, кроме оглавления.
chapterFiles :: IO [FilePath]
chapterFiles = do
  files <- listDirectory bookDir
  pure (sort [bookDir </> f | f <- files, ".md" `isSuffixOf` f, f /= "README.md"])

takeName :: FilePath -> String
takeName = reverse . takeWhile (/= '/') . reverse

-- | Цели всех ссылок вида @[текст](цель)@.
targetsOf :: String -> [String]
targetsOf [] = []
targetsOf ('(' : rest) = takeWhile (/= ')') rest : targetsOf (drop 1 (dropWhile (/= ')') rest))
targetsOf (_ : rest) = targetsOf rest

-- | Ссылки на главы из оглавления: @[текст](01-first-sound.md)@.
linksOf :: String -> [String]
linksOf src = [l | l <- targetsOf src, ".md" `isSuffixOf` l, '/' `notElem` l, l /= "README.md"]

-- | Якоря, на которые главы ссылаются: @glossary.md#nyquist@.
anchorsOf :: String -> [String]
anchorsOf = mapMaybe (stripPrefix "glossary.md#") . targetsOf

-- | Якоря, объявленные в глоссарии: @\<a id="nyquist"\>\</a\>@.
idsOf :: String -> [String]
idsOf src = case breakOn open src of
  Nothing -> []
  Just rest -> takeWhile (/= '"') rest : idsOf (dropWhile (/= '"') rest)
  where
    open = "<a id=\""
    breakOn pat s
      | pat `isPrefixOf` s = Just (drop (length pat) s)
      | otherwise = case s of
          [] -> Nothing
          _ : more -> breakOn pat more

-- | Блок кода в главе. Без символа сверяется весь файл целиком: так
-- показывают маленькие примеры вроде tracks\/Minimal.hs.
data Block = Block
  { blockFile :: FilePath
  , blockSym :: Maybe String
  , blockCode :: [String]
  , blockLine :: Int
  }

checkChapter :: FilePath -> IO [String]
checkChapter chapter = do
  blocks <- blocksOf <$> readFile chapter
  concat <$> forM blocks compare'
  where
    compare' b = do
      let path = blockFile b
      ok <- doesFileExist path
      if not ok
        then pure [at b <> ": нет файла " <> path]
        else do
          src <- lines <$> readFile path
          pure $ case maybe (Just (trimEnd src)) (`defOf` src) (blockSym b) of
            Nothing -> [at b <> ": в " <> path <> " нет " <> fromMaybe "?" (blockSym b)]
            Just def
              | def == trimEnd (blockCode b) -> []
              | otherwise ->
                  [ at b <> ": блок разошёлся с " <> path <> "\nв книге:\n" <> unlines (blockCode b) <> "в коде:\n" <> unlines def
                  ]
    at b = chapter <> ":" <> show (blockLine b)

-- | Блоки haskell с пометкой файла и символа.
blocksOf :: String -> [Block]
blocksOf src = go (zip [1 ..] (lines src))
  where
    go [] = []
    go ((i, l) : rest) = case marked l of
      Just (f, s) ->
        let (body, more) = break (isFence . snd) rest
         in Block f s (map snd body) i : go (drop 1 more)
      Nothing -> go rest

isFence :: String -> Bool
isFence = isPrefixOf "```"

-- | @```haskell file=book\/Book\/Ch01.hs sym=tone@; без @sym@ сверяется весь
-- файл.
marked :: String -> Maybe (FilePath, Maybe String)
marked l = do
  attrs <- words <$> stripPrefix "```haskell " l
  f <- listToMaybe (mapMaybe (stripPrefix "file=") attrs)
  pure (f, listToMaybe (mapMaybe (stripPrefix "sym=") attrs))

-- | Непомеченные блоки haskell: их никто не проверяет.
looseBlocks :: FilePath -> IO [String]
looseBlocks chapter = do
  ls <- zip [1 :: Int ..] . lines <$> readFile chapter
  pure [chapter <> ":" <> show i | (i, l) <- ls, l == "```haskell"]

-- | Определение символа: строка, начинающаяся с его имени, и всё, что ниже
-- с отступом, до следующего определения верхнего уровня. Хаддок не берём:
-- в книге его роль играет проза.
--
-- Тип ищется и по объявлению (data Env, newtype Sig): глава цитирует не
-- только функции.
defOf :: String -> [String] -> Maybe [String]
defOf sym src = case dropWhile (not . starts) src of
  [] -> Nothing
  x : rest -> Just (trimEnd (x : takeWhile continues rest))
  where
    heads = sym : [kind <> " " <> sym | kind <- ["data", "newtype", "type"]]
    starts l = any (`beginsWith` l) heads
    beginsWith h l = case stripPrefix h l of
      Just (c : _) -> c == ' '
      _ -> False
    continues l = null l || " " `isPrefixOf` l || starts l

trimEnd :: [String] -> [String]
trimEnd = reverse . dropWhile null . reverse
