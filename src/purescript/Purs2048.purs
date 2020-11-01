module Purs2048 where
      
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Category (identity)
import Data.Boolean (otherwise)
import Data.Eq (class Eq, eq, (/=), (==))
import Data.Foldable (foldl, length)
import Data.Function (($))
import Data.Functor (class Functor, map)
import Data.HeytingAlgebra ((&&), (||))
import Data.List (filter, take, reverse, transpose, zip, snoc, concatMap, index)
import Data.List.Types (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((+), (*))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (randomInt)

-- ---------------------------------------------------------------

data A4 a = A4 a a a a

instance equalA4 :: (Eq a) => Eq (A4 a) where
    eq (A4 a1 a2 a3 a4) (A4 a1' a2' a3' a4') = (eq a1 a1') && (eq a2 a2') && (eq a3 a3') && (eq a4 a4')

instance showA4 :: (Show a) => Show (A4 a) where
    show (A4 a1 a2 a3 a4) = "A4[" <> show a1 <> ", " <> show a2 <> ", " <> show a3 <> ", " <> show a4 <> "]"

instance functorA4 :: Functor A4 where
    map f (A4 a1 a2 a3 a4) = A4 (f a1) (f a2) (f a3) (f a4)

toList :: forall a. A4 a -> List a
toList (A4 a1 a2 a3 a4) = a1 : a2 : a3 : a4 : Nil

toA4 :: forall a. a -> List a -> A4 a
toA4 u Nil = A4 u u u u
toA4 u (Cons a1 Nil) = A4 a1 u u u
toA4 u (Cons a1 (Cons a2 Nil)) = A4 a1 a2 u u
toA4 u (Cons a1 (Cons a2 (Cons a3 Nil))) = A4 a1 a2 a3 u
toA4 u (Cons a1 (Cons a2 (Cons a3 (Cons a4 _)))) = A4 a1 a2 a3 a4

-- data List a = Nil | Cons a (List a)

fillA4 :: forall a. a -> A4 a
fillA4 v = A4 v v v v

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

data Pow2 = Base | Raise Pow2    -- Raise (Raise Base )  = 2^3
derive instance equalPow2 :: Eq Pow2

instance showPow2 :: Show Pow2 where
    show p = show $ pow2ToInt p

pow2ToInt :: Pow2 -> Int
pow2ToInt  Base     = 2
pow2ToInt (Raise p) = (pow2ToInt p) * 2

data TileValue = EmptyTile | TileValue Pow2

newValues :: List Pow2
newValues = Base : (Raise Base) : Nil

instance showTileValue :: Show TileValue where
    show  EmptyTile    = ""
    show (TileValue p) = show p

instance equalTileValue :: Eq TileValue where
    eq  EmptyTile     EmptyTile     = true
    eq (TileValue p) (TileValue p') = eq p p'
    eq  _             _             = false

type RowCol = A4 TileValue

emptyRow :: RowCol
emptyRow = fillA4 EmptyTile 

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

type Grid = A4 RowCol

emptyGrid :: Grid
emptyGrid = fillA4 emptyRow

-- https://www.mathsisfun.com/definitions/transpose-matrix-.html

-- g = A4 RowCol = A4 A4 Int -> toList -> List(A4 Int) -> map toList -> List(List(Int)) -> map toA4 0 -> List(A4 Int) -> map toA4 emptyRow -> A4 A4 Int = Grid

transposeGrid :: Grid -> Grid
transposeGrid g = toA4 emptyRow (map (toA4 EmptyTile) (transpose (map toList (toList g))))

reverseRowCol :: RowCol -> RowCol
reverseRowCol (A4 a1 a2 a3 a4) = (A4 a4 a3 a2 a1)

reverseGrid :: Grid -> Grid
reverseGrid g = toA4 emptyRow (map reverseRowCol (toList g))

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

shift :: RowCol -> RowCol
shift a = fill (filter (_ /= EmptyTile) (toList a))
-- A4 0 2 0 2 -> [0, 2, 0, 2] -> [2, 2] -> A4 2 2 0 0

fill :: List TileValue -> RowCol
fill xs = toA4 EmptyTile xs
-- fill xs = toA4 0 (xs <> (toList emptyRow))

raiseTile :: TileValue -> TileValue 
raiseTile  EmptyTile    = EmptyTile
raiseTile (TileValue p) = TileValue (Raise p)
-- combineTile :: TileValue -> TileValue -> Either (Tuple TileValue TileValue) TileValue
-- data Either a b = Left a | Right b
-- Left for error, Right for Success

compact :: RowCol -> RowCol
compact a = case a' of
        (A4 a1 a2 a3 a4) | (a1 == a2 && a3 == a4)   -> (fill ((raiseTile a1) : (raiseTile a3) : Nil )) -- 2 2 4 4 -> 4 8 0 0
                         | (a1 == a2)               -> (fill ((raiseTile a1) : a3 : a4 : Nil ))        -- 2 2 4 8 -> 4 4 8 0
                         | (a2 == a3)               -> (fill (a1 : (raiseTile a2) : a4 : Nil ))        -- 2 4 4 8 -> 2 8 8 0
                         | (a3 == a4)               -> (fill (a1 : a2 : (raiseTile a3) : Nil ))        -- 2 4 8 8 -> 2 4 16 0
                         | otherwise                -> a'
    where
        a' = shift a

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

data Command = Up | Down | Left | Right

applyCommand :: Command -> Grid -> Grid
applyCommand Left  g = toA4 emptyRow (map compact (toList g))
applyCommand Right g = reverseGrid (applyCommand Left (reverseGrid g))
applyCommand Up    g = transposeGrid (applyCommand Left  (transposeGrid g))
applyCommand Down  g = transposeGrid (applyCommand Right (transposeGrid g))

canCompact :: Grid -> Boolean
canCompact grid =   (grid /= (applyCommand Left  grid))
                ||  (grid /= (applyCommand Right grid))
                ||  (grid /= (applyCommand Up    grid))
                ||  (grid /= (applyCommand Down  grid))

-- ---------------------------------------------------------------

data A4Index = I1 | I2 | I3 | I4

derive instance eqA4Index :: Eq A4Index

instance showA4Index :: Show A4Index where
    show I1 = "1"
    show I2 = "2"
    show I3 = "3"
    show I4 = "4"

a4Index :: List A4Index
a4Index = I1 : I2 : I3 : I4 : Nil -- :: List A4Index

indexA4 :: forall a. A4 a -> List (Tuple A4Index a)
indexA4 v = zip a4Index (toList v)

a4IndexFromInt :: Int -> A4Index
a4IndexFromInt 1 = I1
a4IndexFromInt 2 = I2
a4IndexFromInt 3 = I3
a4IndexFromInt 4 = I4
a4IndexFromInt _ = I4

-- ---------------------------------------------------------------

data Tile = Tile A4Index A4Index TileValue

instance equalTile :: Eq Tile where
  eq (Tile row1 col1 value1) (Tile row2 col2 value2) = (eq row1 row2) && (eq col1 col2) && (eq value1 value2)

instance showTile :: Show Tile where
  show (Tile row col value) = "Tile[" <> show row <> ", " <> show col <> " => " <> show value <> "]"

tileValues :: Grid -> List Tile
tileValues g = concatMap rowTiles indexedRows
    where
        -- Tuple I1 [Tuple (I1 1), Tuple(I2, 2), Tuple(I3, 3), Tuple(I4, 4)] -> List (Tile(I1, I1, 1), Tile...) 
        rowTiles :: Tuple A4Index (List (Tuple A4Index TileValue)) -> List Tile
        -- rowTiles (Tuple row cols) = foldl snoc Nil (map (\(Tuple col value) -> Tile row col value) cols)
        rowTiles (Tuple row cols) = map (\(Tuple col value) -> Tile row col value) cols
        indexedRows = zip a4Index rows  :: List (Tuple A4Index (List (Tuple A4Index TileValue)))
        rows = map indexA4 (toList g)

emptyTiles :: Grid -> List Tile
emptyTiles g = filter (\(Tile _ _ value) -> value == EmptyTile) (tileValues g)

insertTile :: Tile -> Grid -> Grid
insertTile (Tile row column value) grid = setValue row updatedRow grid
    where
        updatedRow = setValue column value (getValue row grid) :: A4 TileValue

        getValue :: forall a. A4Index -> A4 a -> a
        getValue I1 (A4 v1 v2 v3 v4) = v1
        getValue I2 (A4 v1 v2 v3 v4) = v2
        getValue I3 (A4 v1 v2 v3 v4) = v3
        getValue I4 (A4 v1 v2 v3 v4) = v4

        setValue :: forall a. A4Index -> a -> A4 a -> A4 a
        setValue I1 v (A4 v1 v2 v3 v4) = A4 v  v2 v3 v4
        setValue I2 v (A4 v1 v2 v3 v4) = A4 v1 v  v3 v4
        setValue I3 v (A4 v1 v2 v3 v4) = A4 v1 v2 v  v4
        setValue I4 v (A4 v1 v2 v3 v4) = A4 v1 v2 v3 v


addTile :: Grid -> Effect Grid
addTile g = do -- >>=
    let empties = emptyTiles g :: List Tile
    tileIndex :: Int <- randomInt 0 ((length empties) - 1)
    tileValueIndex :: Int <- randomInt 0 ((length newValues) - 1)
    let tileValue = TileValue (maybe Base identity (index newValues tileValueIndex))
    let tile = index empties tileIndex :: Maybe Tile
    pure $ case tile of
        Just (Tile row column _) -> insertTile (Tile row column tileValue) g
        Nothing                  -> g
    -- pure $ maybe g (\(Tile row column _) -> insertTile (Tile row column tileValue) g) (index empties tileIndex)

-- data Maybe a = Nothing | Just a

-- pure :: forall a f. Applicative f => a -> f a

-- ---------------------------------------------------------------
