-- | Общее для примеров книги: как пример главы превращается в файл.
--
-- Стемов и кэша тут нет намеренно: пример это один сигнал, который надо
-- послушать, а не трек. Про стемы говорит глава 9.
module Book.Prelude
  ( Example (..)
  , example
  , exampleWide
  , renderExamples
  ) where

import Control.Monad (when)
import Data.List (group, sort)
import Sound.Sig
import System.Directory (removeFile)
import System.FilePath ((</>))

-- | Пример главы: имя файла без расширения и звук.
data Example = Example
  { exName :: String
  , exSound :: Either Sig Stereo
  }

-- | Пример в моно: так пишется всё, где стерео не при чём.
example :: String -> Sig -> Example
example name = Example name . Left

-- | Пример в стерео.
exampleWide :: String -> Stereo -> Example
exampleWide name = Example name . Right

-- | Пишет примеры в каталог и докладывает о каждом.
--
-- Совпавшие имена это ошибка: второй пример молча затёр бы первый, а
-- заметить это можно только на слух.
-- | Потолок длины примера. Забытое окно иначе пишется до предела WAV в
-- 4 ГиБ: минуты счёта, гигабайты на диске и сообщение не по делу. Примеры
-- книги короткие, поэтому потолок низкий и ошибка стоит секунду.
maxExampleSec :: Double
maxExampleSec = 30

renderExamples :: FilePath -> [Example] -> IO ()
renderExamples dir examples = do
  case duplicates (map exName examples) of
    [] -> pure ()
    dups -> ioError (userError ("hsig: примеры с одним именем: " <> unwords dups))
  mapM_ one examples
  where
    one e = do
      let path = dir </> exName e <> ".wav"
          -- На сэмпл больше потолка: пример ровно в потолок законен.
          capped = takeSec (maxExampleSec + 1 / envRate defaultEnv)
          limit = round (maxExampleSec * envRate defaultEnv)
      report <- case exSound e of
        Left sig -> writeWav defaultEnv Bits16 path (capped sig)
        Right (Stereo l r) -> writeWavStereo defaultEnv Bits16 path (capped l) (capped r)
      when (clipFrames report > limit) $ do
        removeFile path
        ioError (userError (exName e <> ": пример длиннее " <> show maxExampleSec <> " с, не забыто ли окно (gate)?"))
      putStrLn path

-- | Значения, встретившиеся больше одного раза.
duplicates :: [String] -> [String]
duplicates = concatMap (take 1) . filter ((> 1) . length) . group . sort
