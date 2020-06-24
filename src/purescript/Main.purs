module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Purs2048 (Grid, emptyGrid, addTile, A4(..))
import Widgets.Purs2048 as Purs2048Widget
import Concur.React.Run (runWidgetInDom)

main :: Effect Unit
main = do
    let empty = emptyGrid ::Grid
    gridWithOneTile  :: Grid <- addTile empty
    gridWithTwoTiles :: Grid <- addTile gridWithOneTile
    -- let gridWithTwoTiles = (A4
    --             (A4  16    8   64 128)
    --             (A4 256  512 1024   4)
    --             (A4   0 2048    8   2)
    --             (A4   2 4096    4   4)
    --         )
    runWidgetInDom "app" (Purs2048Widget.widget gridWithTwoTiles)
