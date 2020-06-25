module Test.Main where

import Data.Unit (Unit)
import Effect (Effect)
import Test.Unit.Main (runTest)
import Test.Purs2048 as Test.Purs2048

main :: Effect Unit
main = runTest do
  Test.Purs2048.purs2048TestSuite

