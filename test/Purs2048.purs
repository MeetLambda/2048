module Test.Purs2048 where

import Control.Bind (discard)
import Control.Monad.Free (Free)
import Data.List.Types (List(..), (:))
import Data.Semigroup ((<>))
import Data.Unit (Unit)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Purs2048 (A4(..), shift, RowCol(..), Orientation(..), Direction(..), toList, toA4, collapse, applyCommand, Command(..))

purs2048TestSuite :: Free TestF Unit
purs2048TestSuite =
    suite "Purs2048 Test Suite" do
        test "list handling" do
            -- Assert.assert      "2 + 2 should be 4"    $ (2 + 2) == 4
            -- Assert.assertFalse "2 + 2 shouldn't be 5" $ (2 + 2) == 5
            -- Assert.equal       4 (2 + 2)
            -- Assert.expectFailure "2 + 2 shouldn't be 5" $ Assert.equal 5 (2 + 2)
            Assert.equal (4 : 2 : 0 : 0 : Nil) (toList (A4 4 2 0 0))
            Assert.equal (A4 4 2 0 0) (toA4 0 (4 : 2 : 0 : 0 : Nil))
        
        test "test shift" do    
            Assert.equal (A4 4 2 0 0) (shift L (A4 4 2 0 0))
            Assert.equal (A4 4 2 0 0) (shift L (A4 0 4 0 2))
            Assert.equal (A4 4 0 0 0) (shift L (A4 0 4 0 0))
            Assert.equal (A4 0 0 0 0) (shift L (A4 0 0 0 0))
            Assert.equal (A4 2 4 0 0) (shift L (A4 0 0 2 4))
            Assert.equal (A4 0 0 2 4) (shift R (A4 0 0 2 4))
            Assert.equal (A4 0 0 4 2) (shift R (A4 4 0 2 0))
        
        test "test collapse" do
            Assert.equal (A4 0 0 4 2) (collapse R (A4 4 0 2 0))
            Assert.equal (A4 0 0 4 4) (collapse R (A4 2 2 2 2))
            Assert.equal (A4 0 0 4 4) (collapse R (A4 0 4 2 2))

            Assert.equal (A4 4 4 0 0) (collapse L (A4 2 2 2 2))
            Assert.equal (A4 2 4 4 0) (collapse L (A4 2 4 2 2))
            Assert.equal (A4 8 0 0 0) (collapse L (A4 0 4 0 4))
            Assert.equal (A4 4 4 2 0) (collapse L (A4 4 2 2 2))

        test "test apply command" do
            Assert.equal (A4
                (A4 0 0 0 4)
                (A4 0 0 4 2)
                (A4 0 0 8 2)
                (A4 0 2 4 2)
            ) (applyCommand Right (A4
                (A4 2 0 2 0)
                (A4 4 0 2 0)
                (A4 4 4 2 0)
                (A4 2 4 2 0)
            ))
            Assert.equal (A4
                (A4 0 2 4 4)
                (A4 0 0 8 4)
                (A4 0 0 4 2)
                (A4 0 0 0 0)
            ) (applyCommand Up (A4
                (A4 0 0 0 4)
                (A4 0 0 4 2)
                (A4 0 0 8 2)
                (A4 0 2 4 2)
            ))
            Assert.equal (A4
                (A4 0 0 0 0)
                (A4 0 0 4 4)
                (A4 0 0 8 2)
                (A4 0 2 4 4)
            ) (applyCommand Down (A4
                (A4 0 0 0 4)
                (A4 0 0 4 2)
                (A4 0 0 8 2)
                (A4 0 2 4 2)
            ))
            -- Assert.equal (A4 0 0 4 4) (collapse R (A4 2 2 2 2))
            -- Assert.equal (A4 0 0 4 4) (collapse R (A4 0 4 2 2))

            -- Assert.equal (A4 4 4 0 0) (collapse L (A4 2 2 2 2))
            -- Assert.equal (A4 2 4 4 0) (collapse L (A4 2 4 2 2))
            -- Assert.equal (A4 8 0 0 0) (collapse L (A4 0 4 0 4))