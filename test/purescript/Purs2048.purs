module Test.Purs2048 where

import Control.Bind (discard)
import Control.Monad.Free (Free)
import Data.Functor (map)
import Data.List.Types (List(..), (:))
import Data.Unit (Unit)
import Purs2048 (A4(..), A4Index(..), Command(..), Tile(..), TileValue(..), Pow2(..), applyCommand, canCompact, compact, emptyTiles, insertTile, shift, tileValues, toA4, toList)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert

toPow2 :: Int -> Pow2
toPow2  2 = Base
toPow2  4 = Raise Base
toPow2  8 = Raise (Raise Base)
toPow2 16 = Raise (Raise (Raise Base))
toPow2  _ = Base -- This is not a valid general solution; useful for tests though.

toTileValue :: Int -> TileValue
toTileValue 0 = EmptyTile
toTileValue i = TileValue (toPow2 i)

tile :: A4Index -> A4Index -> Int -> Tile
tile x y i = Tile x y (toTileValue i)

tiles :: A4 Int -> A4 TileValue
tiles a = map toTileValue a

grid :: A4 (A4 Int) -> A4 (A4 TileValue)
grid g = map tiles g

purs2048TestSuite :: Free TestF Unit
purs2048TestSuite =
    suite "Purs2048 Test Suite" do
        test "list handling" do
            Assert.equal (4 : 2 : 0 : 0 : Nil) (toList (A4 4 2 0 0))
            Assert.equal (A4 4 2 0 0) (toA4 0 (4 : 2 : 0 : 0 : Nil))
        
        test "test shift" do    
            Assert.equal (tiles (A4 4 2 0 0)) (shift (tiles (A4 4 2 0 0)))
            Assert.equal (tiles (A4 4 2 0 0)) (shift (tiles (A4 0 4 0 2)))
            Assert.equal (tiles (A4 4 0 0 0)) (shift (tiles (A4 0 4 0 0)))
            Assert.equal (tiles (A4 0 0 0 0)) (shift (tiles (A4 0 0 0 0)))
            Assert.equal (tiles (A4 2 4 0 0)) (shift (tiles (A4 0 0 2 4)))
        
        test "test compact" do
            Assert.equal (tiles (A4 4 4 0 0)) (compact (tiles (A4 2 2 2 2)))
            Assert.equal (tiles (A4 2 4 4 0)) (compact (tiles (A4 2 4 2 2)))
            Assert.equal (tiles (A4 8 0 0 0)) (compact (tiles (A4 0 4 0 4)))
            Assert.equal (tiles (A4 4 4 2 0)) (compact (tiles (A4 4 2 2 2)))

        test "test apply command" do
            Assert.equal (grid (A4
                (A4 0 0 0 4)
                (A4 0 0 4 2)
                (A4 0 0 8 2)
                (A4 0 2 4 2)
            )) (applyCommand Right (grid (A4
                (A4 2 0 2 0)
                (A4 4 0 2 0)
                (A4 4 4 2 0)
                (A4 2 4 2 0)
            )))
            Assert.equal (grid (A4
                (A4 0 2 4 4)
                (A4 0 0 8 4)
                (A4 0 0 4 2)
                (A4 0 0 0 0)
            )) (applyCommand Up (grid (A4
                (A4 0 0 0 4)
                (A4 0 0 4 2)
                (A4 0 0 8 2)
                (A4 0 2 4 2)
            )))
            Assert.equal (grid (A4
                (A4 0 0 0 0)
                (A4 0 0 4 4)
                (A4 0 0 8 2)
                (A4 0 2 4 4)
            )) (applyCommand Down (grid (A4
                (A4 0 0 0 4)
                (A4 0 0 4 2)
                (A4 0 0 8 2)
                (A4 0 2 4 2)
            )))

        test "test tileValues" do
            Assert.equal (
                (tile I1 I1 2) :
                (tile I1 I2 0) :
                (tile I1 I3 2) :
                (tile I1 I4 0) :

                (tile I2 I1 4) :
                (tile I2 I2 0) :
                (tile I2 I3 2) :
                (tile I2 I4 0) :

                (tile I3 I1 4) :
                (tile I3 I2 4) :
                (tile I3 I3 2) :
                (tile I3 I4 0) :

                (tile I4 I1 2) :
                (tile I4 I2 4) :
                (tile I4 I3 2) :
                (tile I4 I4 0) :
                Nil
            ) (tileValues (grid (A4
                (A4 2 0 2 0)
                (A4 4 0 2 0)
                (A4 4 4 2 0)
                (A4 2 4 2 0)
            )))

        test "test empty tiles" do
            Assert.equal (
                (tile I1 I2 0) :
                (tile I1 I4 0) :
                (tile I2 I2 0) :
                (tile I2 I4 0) :
                (tile I3 I4 0) :
                (tile I4 I4 0) :
                Nil
            ) (emptyTiles (grid (A4
                (A4 2 0 2 0)
                (A4 4 0 2 0)
                (A4 4 4 2 0)
                (A4 2 4 2 0)
            )))
            Assert.equal (
                (tile I1 I2 0) :
                Nil
            ) (emptyTiles (grid (A4
                (A4 2 0 2 2)
                (A4 4 2 2 2)
                (A4 4 4 2 2)
                (A4 2 4 2 2)
            )))

        test "test insert tyle" do
            Assert.equal (grid (A4
                (A4 4 0 0 0)
                (A4 4 2 2 0)
                (A4 8 2 0 0)
                (A4 2 4 2 0)
            )) (insertTile (tile I2 I3 2) (grid (A4
                (A4 4 0 0 0)
                (A4 4 2 0 0)
                (A4 8 2 0 0)
                (A4 2 4 2 0)
            )))

        test "test canCompact" do
            Assert.equal false (canCompact (grid (A4
                (A4 4 2 4 2)
                (A4 2 4 2 4)
                (A4 4 2 4 2)
                (A4 2 4 2 4)
            )))

            Assert.equal true (canCompact (grid (A4
                (A4 4 2 8 8)
                (A4 2 4 2 4)
                (A4 4 2 4 2)
                (A4 2 4 2 4)
            )))
