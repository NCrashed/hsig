-- | Точка входа тестов.
module Main (main) where

import CoreSpec qualified
import DelaySpec qualified
import EnvelopeSpec qualified
import FilterSpec qualified
import NonlinSpec qualified
import OscSpec qualified
import RandomSpec qualified
import RenderSpec qualified
import ResampleSpec qualified
import ScoreSpec qualified
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
      , ResampleSpec.tests
      , NonlinSpec.tests
      , DelaySpec.tests
      , ScoreSpec.tests
      , RenderSpec.tests
      , WavSpec.tests
      ]
