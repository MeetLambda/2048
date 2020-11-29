module Widgets.Purs2048 where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text, h2, h4, a, p, span, button)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Data.Array (cons)
import Data.Eq (eq)
import Data.Function (($))
import Data.Functor (map, ($>))
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Purs2048 (Grid, TileValue(..), Tile(..), Command(..), tileValues, applyCommand, addTile, canCompact)
import React.SyntheticEvent (SyntheticKeyboardEvent, key)

data GameStatus = KEEP_PLAYING | GAME_OVER

cssValue :: TileValue -> String
cssValue EmptyTile = "value_0"
cssValue (TileValue p)  = "value_" <> show p

gridWidget :: Grid -> GameStatus -> Widget HTML (Maybe Command)
gridWidget grid status = div [Props.className "frame"] [
    h4 [] [text "Meetlambda 2048"],
    div [Props.className "grid"] (map (
        \(Tile row col value) ->
            -- div [Props.className ("tile value_" <> (show value))] [
            div [Props.className ("tile " <> (cssValue value))] [
                span [] [text (show value)]
            ]
    ) (toArray  $ tileValues grid)),
    div [Props.className "buttons"] [
        button [Props.onClick, Props.className "button up"] [text "UP"] $> (Just Up),
        button [Props.onClick, Props.className "button down"] [text "DOWN"] $> (Just Down),
        button [Props.onClick, Props.className "button right"] [text "RIGHT"] $> (Just Right),
        button [Props.onClick, Props.className "button left"] [text "LEFT"] $> (Just Left)
    ],
    (
        case status of
            KEEP_PLAYING -> h2 [] [text ""]
            GAME_OVER    -> h2 [] [text "Game Over"]
    ),
    div [Props.className "info"] [
        p [Props.className "instructions"] [text "[you can also use arrow keys to play]"],
        p [Props.className "repository"] [
            text "GitHub ",
            a [Props.href "https://github.com/MeetLambda/2048"] [text "repository"]
        ]
    ]
]

widget :: forall a. Grid -> Widget HTML a
widget initialGrid = do
    liftEffect startListeningToKeyboardEvents
    go initialGrid KEEP_PLAYING
    where
        go :: Grid -> GameStatus -> Widget HTML a
        go grid status = do
            -- event   :: SyntheticKeyboardEvent   <- liftAff awaitKey <|> gridWidget grid status
            -- command :: Maybe Command            <- liftEffect $ toCommand event
            command :: Maybe Command               <- (liftAff awaitKey >>= (\event -> liftEffect $ toCommand event)) <|> gridWidget grid status
            case command of
                Nothing             -> go grid status
                Just actualCommand  -> do
                    let compactGrid = applyCommand actualCommand grid :: Grid
                    case eq grid compactGrid of
                        true  -> go grid status
                        false -> do
                            newGrid :: Grid <- liftEffect $ addTile compactGrid
                            let newStatus = if (canCompact newGrid) then KEEP_PLAYING else GAME_OVER
                            go newGrid newStatus
                
-- ======================================================================

toArray :: forall a. List a -> Array a
toArray Nil = []
toArray (Cons x xs) = cons x (toArray xs)

-- ----------------------------------------------------------------------

toCommand :: SyntheticKeyboardEvent -> Effect (Maybe Command)
toCommand event = do
    k <- key event
    pure $ case k of
        "ArrowUp"    -> Just Up
        "ArrowDown"  -> Just Down
        "ArrowLeft"  -> Just Left
        "ArrowRight" -> Just Right
        _ -> Nothing

-- ----------------------------------------------------------------------

startListeningToKeyboardEvents :: Effect Unit
startListeningToKeyboardEvents = startListening

-- ======================================================================
-- 
--  This final part of the code has been copied from, or else heavily inspired to, the sample code
--  on Concur React repository; in particular:
--  - https://github.com/purescript-concur/purescript-concur-react/blob/master/examples/src/Test/Keyboard.purs
--

-- FFI ------------------------------------------------------------

-- Start and stop listening for keyboard events
foreign import startListening :: Effect Unit
foreign import stopListening  :: Effect Unit

-- Await a key input. Requires that we are listening for events.
foreign import _awaitKey :: EffectFnAff SyntheticKeyboardEvent
awaitKey :: Aff SyntheticKeyboardEvent
awaitKey = fromEffectFnAff _awaitKey

-- ======================================================================