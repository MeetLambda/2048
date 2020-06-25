module Main where

import Control.Bind (bind, (=<<))
import Data.Unit (Unit)
import Effect (Effect)
import Purs2048 (emptyGrid, addTile)
import Widgets.Purs2048 as Purs2048Widget
import Concur.React.Run (runWidgetInDom)

main :: Effect Unit
main = do
    grid <- addTile =<< addTile emptyGrid
    runWidgetInDom "app" (Purs2048Widget.widget grid)
