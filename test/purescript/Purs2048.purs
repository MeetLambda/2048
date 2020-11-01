module Test.Purs2048 where

import Control.Bind (discard)
import Control.Monad.Free (Free)
import Data.Int (fromNumber)
import Data.List.Types (List(..), (:))
import Data.Unit (Unit)
import Purs2048 (A4(..), A4Index(..), Command(..), Tile(..), TileValue(..), Pow2, applyCommand, canCompact, compact, emptyTiles, insertTile, shift, tileValues, toA4, toList)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert

-- toPow2 :: Int -> Pow2
-- toPow2 x = 

toTileValue :: Int -> TileValue
toTileValue 0 = EmptyTile
toTileValue x = case fromNumber (log (toNumber x)) of -- TODO: it's the wrong log base
    Just 0  -> EmptyTile
    Just p  -> TileValue (toPow2 p)
    Nothing -> EmptyTile

tiles :: A4 Int -> A4 TileValue
tiles a = map toTileValue a

purs2048TestSuite :: Free TestF Unit
purs2048TestSuite =
    suite "Purs2048 Test Suite" do
        test "list handling" do
            Assert.equal (4 : 2 : 0 : 0 : Nil) (toList (A4 4 2 0 0))
            Assert.equal (A4 4 2 0 0) (toA4 0 (4 : 2 : 0 : 0 : Nil))
        
        test "test shift" do    
            Assert.equal (A4 4 2 0 0) (shift (A4 4 2 0 0))
            Assert.equal (A4 4 2 0 0) (shift (A4 0 4 0 2))
            Assert.equal (A4 4 0 0 0) (shift (A4 0 4 0 0))
            Assert.equal (A4 0 0 0 0) (shift (A4 0 0 0 0))
            Assert.equal (A4 2 4 0 0) (shift (A4 0 0 2 4))
        
        test "test compact" do
            Assert.equal (A4 4 4 0 0) (compact (A4 2 2 2 2))
            Assert.equal (A4 2 4 4 0) (compact (A4 2 4 2 2))
            Assert.equal (A4 8 0 0 0) (compact (A4 0 4 0 4))
            Assert.equal (A4 4 4 2 0) (compact (A4 4 2 2 2))

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
            Assert.equal (
                (Tile I1 I2 0) :
                Nil
            ) (emptyTiles (A4
                (A4 2 0 2 2)
                (A4 4 2 2 2)
                (A4 4 4 2 2)
                (A4 2 4 2 2)
            ))

        test "test insert tyle" do
            Assert.equal (A4
                (A4 4 0 0 0)
                (A4 4 2 2 0)
                (A4 8 2 0 0)
                (A4 2 4 2 0)
            ) (insertTile (Tile I2 I3 2) (A4
                (A4 4 0 0 0)
                (A4 4 2 0 0)
                (A4 8 2 0 0)
                (A4 2 4 2 0)
            ))

        test "test canCompact" do
            Assert.equal false (canCompact (A4
                (A4 4 2 4 2)
                (A4 2 4 2 4)
                (A4 4 2 4 2)
                (A4 2 4 2 4)
            ))

            Assert.equal true (canCompact (A4
                (A4 4 2 8 8)
                (A4 2 4 2 4)
                (A4 4 2 4 2)
                (A4 2 4 2 4)
            ))
