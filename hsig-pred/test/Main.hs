-- | Точка входа тестов подпакета.
module Main (main) where

import ComposeSpec qualified
import DiagramSpec qualified
import DistSpec qualified
import KernelSpec qualified
import ListenerSpec qualified
import MachineSpec qualified
import MetricSpec qualified
import ModelSpec qualified
import OrbifoldSpec qualified
import RenderSpec qualified
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "hsig-pred"
      [ DistSpec.tests
      , ModelSpec.tests
      , MachineSpec.tests
      , KernelSpec.tests
      , ListenerSpec.tests
      , MetricSpec.tests
      , OrbifoldSpec.tests
      , RenderSpec.tests
      , ComposeSpec.tests
      , DiagramSpec.tests
      ]
