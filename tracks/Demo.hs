-- | Демо-трек. На M0 здесь будет рендер синуса 440 Гц в WAV.
module Demo
  ( main
  ) where

import Sound.Sig (version)

main :: IO ()
main = do
  putStrLn ("hsig " ++ version ++ ": скелет собран.")
  putStrLn "Рендер появится на M0, см. docs/DESIGN.md, разд. 11."
