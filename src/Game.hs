module Game
    ( gameLoop
    , initGameLoop
    ) where

import           Actions
import           Board

initGameLoop :: Board
initGameLoop = [[Blank, Wall, Floor, Player, Wall]]

gameLoop :: Action -> Board
gameLoop (Go West) = [[Blank, Wall, Player, Floor, Wall]]
gameLoop (Go East) = [[Blank, Wall, Floor, Player, Wall]]
gameLoop _ = []
