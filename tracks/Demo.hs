-- | Демо-трек. M0: две секунды чистого тона 440 Гц.
module Demo (main) where

import Sound.Sig

tone :: Sig
tone = takeSec 2 (0.5 * sine 440)

main :: IO ()
main = do
  report <- writeWav defaultEnv Bits16 path tone
  putStrLn (path <> ": пик " <> show (clipPeak report))
  where
    path = "out/demo.wav"
