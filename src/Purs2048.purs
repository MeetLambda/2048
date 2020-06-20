module Purs2048 where

import Data.Eq (class Eq, eq, (/=), (==))

import Control.Bind (bind, discard)
import Data.Functor (map)
import Data.List (filter, take, reverse, transpose)
import Data.HeytingAlgebra ((&&))
import Data.List.Types (List(..), (:))
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Show (class Show, show)
import Control.Semigroupoid ((>>>), (<<<))

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

data Orientation = V | H

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
