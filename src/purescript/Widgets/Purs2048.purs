module Widgets.Purs2048 where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text, table, tbody, tr, td, h4, h5, span)
import Concur.React.Props as Props
import React.SyntheticEvent as SyntheticEvent

import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Array (cons)
import Data.Function (($))
import Data.Functor ((<$), map)
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)

import Purs2048 as Purs2048

gridWidget :: Purs2048.Grid -> Widget HTML Purs2048.Command
gridWidget grid = div [Props.className "frame"] [
    h4 [] [text "2048"],
    div [Props.className "grid"] (map (
        \(Purs2048.Tile row col value) ->
            div [Props.className ("tile value_" <> (show value))] [
                span [] [text (show value)]
            ]
    ) (toArray  $ Purs2048.tileValues grid)),
    Purs2048.Up    <$ h5 [Props.onClick] [text "Up"],
    Purs2048.Down  <$ h5 [Props.onClick] [text "Down"],
    Purs2048.Left  <$ h5 [Props.onClick] [text "Left"],
    Purs2048.Right <$ h5 [Props.onClick] [text "Right"]
]

widget :: forall a. Purs2048.Grid -> Widget HTML a
widget grid = do
    command :: Purs2048.Command <- gridWidget grid
    let grid' = Purs2048.applyCommand command grid
    grid'' :: Purs2048.Grid <- liftEffect (Purs2048.addTile grid')
    widget grid''

-- ======================================================================

toArray :: forall a. List a -> Array a
toArray Nil = []
toArray (Cons x xs) = cons x (toArray xs)

-- ======================================================================
-- 
--  This final part of the code has been copied from, or else heavily inspired to, the sample code
--  on Concur React repository; in particular:
--  - https://github.com/purescript-concur/purescript-concur-react/blob/master/examples/src/Test/Keyboard.purs
--

-- FFI ------------------------------------------------------------

-- Start and stop listening for keyboard events
foreign import startListening :: Effect Unit
foreign import stopListening :: Effect Unit

-- Await a key input. Requires that we are listening for events.
foreign import _awaitKey :: EffectFnAff SyntheticEvent.SyntheticKeyboardEvent
awaitKey :: Aff SyntheticEvent.SyntheticKeyboardEvent
awaitKey = fromEffectFnAff _awaitKey



toKey :: SyntheticEvent.SyntheticKeyboardEvent -> Effect (Maybe Purs2048.Command)
toKey event = do
    k <- SyntheticEvent.key event
    pure $ case k of
        "ArrowUp"    -> Just Purs2048.Up
        "ArrowDown"  -> Just Purs2048.Down
        "ArrowLeft"  -> Just Purs2048.Left
        "ArrowRight" -> Just Purs2048.Right
        _ -> Nothing

-- ----------------------------------------------------------------------

-- ======================================================================