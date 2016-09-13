{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Game
    ( initGameLoop
    , gameLoop

    -- re-export from Model
    , GameState
    , RoundResult (..)
    ) where

import           Actions
import           Dungeon
import           Model
import           Control.Eff
import           Control.Eff.State.Lazy
import           Control.Monad (when)

newPlayerHealth :: Int
newPlayerHealth = 10

newPlayer :: String -> Coords -> Player
newPlayer playerName playerPosition = Player playerName newPlayerHealth playerPosition

initGameLoop :: String -> GameState
initGameLoop playerName =
    let playerCoords = Coords (maxDungeonWidth `div` 2) (maxDungeonHeight `div` 2) in
    GameState (dungeonGenerator playerCoords) (newPlayer playerName playerCoords)

gameLoop :: Member (State GameState) e => Action -> Eff e RoundResult
gameLoop (Go dir) = do
    gs <- get
    let direction = actionToDirection dir
        thePlayer = player gs
        theBoard = board gs
    when (canPlayerMove direction theBoard thePlayer) $ modify $ \st ->
        let (newPos, newBoard) = move direction (position thePlayer) theBoard
        in st { board = newBoard, player = thePlayer {position = newPos}}
    return Continue
    where actionToDirection North = north
          actionToDirection South = south
          actionToDirection West = west
          actionToDirection East = east

gameLoop (Meta Quit) =
    return GameOver
gameLoop _ =
    return Continue
