module Game
    ( initGameLoop
    , runGameLoop
    ) where

import           Actions
import           Board
import           Control.Monad.State

type GameLoop = Action -> State GameState RoundResult

newPlayerHealth :: Int
newPlayerHealth = 10

newPlayer :: String -> Player
newPlayer playerName = Player playerName newPlayerHealth

initGameLoop :: String -> GameState
initGameLoop playerName = GameState [[Blank, Wall, Floor, Hero, Wall]] (newPlayer playerName)

runGameLoop :: Action -> GameState -> (RoundResult, GameState)
runGameLoop action = runState (gameLoop action)

gameLoop :: GameLoop
gameLoop (Go West) = do
    modify $ \st ->
        st { board = [[Blank, Wall, Hero, Floor, Wall]] }
    return Continue
gameLoop (Go East) = do
    modify $ \st ->
        st { board = [[Blank, Wall, Floor, Hero, Wall]] }
    return Continue
gameLoop (Meta Quit) =
    return GameOver
gameLoop _ =
    return Continue
