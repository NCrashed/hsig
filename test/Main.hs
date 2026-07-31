-- | Точка входа тестов.
module Main (main) where

import BookSpec qualified
import CoreSpec qualified
import DelaySpec qualified
import DynamicsSpec qualified
import EnvelopeSpec qualified
import FilterSpec qualified
import HrtfSpec qualified
import LeadSpec qualified
import MiniSpec qualified
import NonlinSpec qualified
import OscSpec qualified
import RandomSpec qualified
import ReactorSpec qualified
import RenderSpec qualified
import ResampleSpec qualified
import ScoreSpec qualified
import StereoSpec qualified
import Test.Tasty
import WavSpec qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "hsig"
      [ CoreSpec.tests
      , RandomSpec.tests
      , ReactorSpec.tests
      , OscSpec.tests
      , EnvelopeSpec.tests
      , FilterSpec.tests
      , ResampleSpec.tests
      , NonlinSpec.tests
      , DelaySpec.tests
      , DynamicsSpec.tests
      , ScoreSpec.tests
      , StereoSpec.tests
      , MiniSpec.tests
      , RenderSpec.tests
      , LeadSpec.tests
      , WavSpec.tests
      , HrtfSpec.tests
      , BookSpec.tests
      ]
