-- | Точка входа тестов.
module Main (main) where

import CoreSpec qualified
import EnvelopeSpec qualified
import FilterSpec qualified
import OscSpec qualified
import RandomSpec qualified
import Test.Tasty
import WavSpec qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "hsig"
      [ CoreSpec.tests
      , RandomSpec.tests
      , OscSpec.tests
      , EnvelopeSpec.tests
      , FilterSpec.tests
      , WavSpec.tests
      ]
