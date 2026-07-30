-- | Помощники для тестов, которым нужен настоящий ввод-вывод.
module TestIO (captureStderr) where

import Control.Exception (finally)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (hClose, hFlush, readFile', stderr)
import System.IO.Temp (withSystemTempFile)

-- | Перехватывает stderr на время действия.
captureStderr :: IO a -> IO (a, String)
captureStderr act = withSystemTempFile "hsig-stderr" $ \path h -> do
  saved <- hDuplicate stderr
  hDuplicateTo h stderr
  r <-
    act `finally` do
      hFlush stderr
      hDuplicateTo saved stderr
      hClose saved
  hClose h
  msg <- readFile' path
  pure (r, msg)
