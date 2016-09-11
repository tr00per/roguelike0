{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Game
    ( initGameLoop
    , gameLoop

    -- re-export from Board
    , GameState
    , RoundResult (..)
    ) where

import           Actions
import           Board
import           Control.Eff
import           Control.Eff.State.Lazy

newPlayerHealth :: Int
newPlayerHealth = 10

newPlayer :: String -> Player
newPlayer playerName = Player playerName newPlayerHealth

initGameLoop :: String -> GameState
initGameLoop playerName = GameState [[Blank, Wall, Floor, Hero, Wall]] (newPlayer playerName)

gameLoop :: Member (State GameState) e => Action -> Eff e RoundResult
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
