module Test.Purs2048 where

import Control.Bind (discard)
import Control.Monad.Free (Free)
import Data.List.Types (List(..), (:))
import Data.Unit (Unit)

import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Purs2048 (A4(..), shift, Direction(..), toList, toA4, collapse, applyCommand, Command(..), Tile(..), A4Index(..), tileValues, emptyTiles)

purs2048TestSuite :: Free TestF Unit
purs2048TestSuite =
    suite "Purs2048 Test Suite" do
        test "list handling" do
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

        test "test tileValues" do
            Assert.equal (
                (Tile I1 I1 2) :
                (Tile I1 I2 0) :
                (Tile I1 I3 2) :
                (Tile I1 I4 0) :

                (Tile I2 I1 4) :
                (Tile I2 I2 0) :
                (Tile I2 I3 2) :
                (Tile I2 I4 0) :

                (Tile I3 I1 4) :
                (Tile I3 I2 4) :
                (Tile I3 I3 2) :
                (Tile I3 I4 0) :

                (Tile I4 I1 2) :
                (Tile I4 I2 4) :
                (Tile I4 I3 2) :
                (Tile I4 I4 0) :
                Nil
            ) (tileValues (A4
                (A4 2 0 2 0)
                (A4 4 0 2 0)
                (A4 4 4 2 0)
                (A4 2 4 2 0)
            ))

        test "test empty tiles" do
            Assert.equal (
                (Tile I1 I2 0) :
                (Tile I1 I4 0) :
                (Tile I2 I2 0) :
                (Tile I2 I4 0) :
                (Tile I3 I4 0) :
                (Tile I4 I4 0) :
                Nil
            ) (emptyTiles (A4
                (A4 2 0 2 0)
                (A4 4 0 2 0)
                (A4 4 4 2 0)
                (A4 2 4 2 0)
            ))
