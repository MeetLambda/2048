module Purs2048 where

import Data.Eq (class Eq, eq, (/=), (==))
import Data.Foldable (foldl)
import Data.Functor (map)
import Data.HeytingAlgebra ((&&))
import Data.List (filter, take, reverse, transpose, zip, snoc, concatMap)
import Data.List.Types (List(..), (:))
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))

data A4 a = A4 a a a a

instance equalA4 :: (Eq a) => Eq (A4 a) where
  eq (A4 a1 a2 a3 a4) (A4 a1' a2' a3' a4') = (eq a1 a1') && (eq a2 a2') && (eq a3 a3') && (eq a4 a4')

instance showA4 :: (Show a) => Show (A4 a) where
  show (A4 a1 a2 a3 a4) = "A4[" <> show a1 <> ", " <> show a2 <> ", " <> show a3 <> ", " <> show a4 <> "]"

toList :: forall a. A4 a -> List a
toList (A4 a1 a2 a3 a4) = a1 : a2 : a3 : a4 : Nil

toA4 :: forall a. a -> List a -> A4 a
toA4 u Nil = A4 u u u u
toA4 u (Cons a1 Nil) = A4 a1 u u u
toA4 u (Cons a1 (Cons a2 Nil)) = A4 a1 a2 u u
toA4 u (Cons a1 (Cons a2 (Cons a3 Nil))) = A4 a1 a2 a3 u
toA4 u (Cons a1 (Cons a2 (Cons a3 (Cons a4 _)))) = A4 a1 a2 a3 a4


type RowCol = A4 Int
type Grid = A4 RowCol

data Command = Up | Down | Left | Right

data Direction = L | R

initGrid :: Grid
initGrid = (A4 emptyRow emptyRow emptyRow emptyRow)

fill :: Direction -> List Int -> RowCol
fill d xs = case d of
    L -> toA4 0 (xs <> (toList emptyRow))
    R -> toA4 0 (reverse (take 4 (reverse xs <> (toList emptyRow))))

shift :: Direction -> RowCol -> RowCol
shift d a = fill d (filter (_ /= 0) (toList a))

collapse :: Direction -> RowCol -> RowCol
collapse d a = case d of
    L -> case a' of
        (A4 a1 a2 a3 a4) | (a1 == a2 && a3 == a4) -> (fill d ((a1 + a2) : (a3 + a4) : Nil ))
        (A4 a1 a2 a3 a4) | (a1 == a2) -> (fill d ((a1 + a2) : a3 : a4 : Nil ))
        (A4 a1 a2 a3 a4) | (a2 == a3) -> (fill d (a1 : (a2 + a3) : a4 : Nil ))
        (A4 a1 a2 a3 a4) | (a3 == a4) -> (fill d (a1 : a2 : (a3 + a4) : Nil ))
        _ -> a'
    R -> case a' of
        (A4 a1 a2 a3 a4) | (a1 == a2 && a3 == a4) -> (fill d ((a1 + a2) : (a3 + a4) : Nil ))
        (A4 a1 a2 a3 a4) | (a3 == a4) -> (fill d (a1 : a2 : (a3 + a4) : Nil ))
        (A4 a1 a2 a3 a4) | (a2 == a3) -> (fill d (a1 : (a2 + a3) : a4 : Nil ))
        (A4 a1 a2 a3 a4) | (a1 == a2) -> (fill d ((a1 + a2) : a3 : a4 : Nil ))
        _ -> a'
    where
        a' = shift d a
    
emptyRow = A4 0 0 0 0 :: A4 Int

transposeGrid :: Grid -> Grid
transposeGrid g = toA4 emptyRow (map (toA4 0) (transpose (map toList (toList g))))

applyCommand :: Command -> Grid -> Grid
applyCommand Left  g = toA4 emptyRow (map (collapse L) (toList g))
applyCommand Right g = toA4 emptyRow (map (collapse R) (toList g))
applyCommand Up    g = transposeGrid (applyCommand Left (transposeGrid g))
applyCommand Down  g = transposeGrid (applyCommand Right (transposeGrid g))

-- ---------------------------------------------------------------

data A4Index = I1 | I2 | I3 | I4
instance equalA4Index :: Eq A4Index where
  eq i1 i2 = eq (show i1) (show i2)

instance showA4Index :: Show A4Index where
  show I1 = "1"
  show I2 = "2"
  show I3 = "3"
  show I4 = "4"

a4Index = I1 : I2 : I3 : I4 : Nil :: List A4Index

index :: forall a. A4 a -> List (Tuple A4Index a)
index v = zip a4Index (toList v)

-- ---------------------------------------------------------------

data Tile = Tile A4Index A4Index Int

instance equalTile :: Eq Tile where
  eq (Tile row1 col1 value1) (Tile row2 col2 value2) = (eq row1 row2) && (eq col1 col2) && (eq value1 value2)

instance showTile :: Show Tile where
  show (Tile row col value) = "Tile[" <> show row <> ", " <> show col <> " => " <> show value <> "]"

-- ---------------------------------------------------------------

-- getIndex :: forall a. A4Index -> A4 a -> a
-- getIndex I1 (A4 a1 a2 a3 a4) = a1
-- getIndex I2 (A4 a1 a2 a3 a4) = a2
-- getIndex I3 (A4 a1 a2 a3 a4) = a3
-- getIndex I4 (A4 a1 a2 a3 a4) = a4


tileValues :: Grid -> List Tile
tileValues g = concatMap rowTiles indexedRows
    where
        rowTiles :: Tuple A4Index (List (Tuple A4Index Int)) -> List Tile
        rowTiles (Tuple row cols) = foldl snoc Nil (map (\(Tuple col value) -> Tile row col value) cols)
        indexedRows = zip a4Index rows  :: List (Tuple A4Index (List (Tuple A4Index Int)))
        rows = map index (toList g)


emptyTiles :: Grid -> List Tile
emptyTiles g = filter (\(Tile _ _ value) -> value == 0) (tileValues g)

-- insertTile :: Int -> Coordinate -> Grid -> Grid
-- insertTile