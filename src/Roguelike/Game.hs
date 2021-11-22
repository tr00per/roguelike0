{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Roguelike.Game
    ( initGameLoop
    , gameLoop

    -- re-export from Model
    , GameState
    , RoundResult (..)
    ) where

import           Control.Eff
import           Control.Eff.State.Lazy
import           Control.Monad          (when)
import           Roguelike.Actions
import           Roguelike.Dungeon
import           Roguelike.Model
import           Roguelike.Random       (RNG)

newPlayerHealth :: Int
newPlayerHealth = 10

newPlayer :: String -> Coords -> Player
newPlayer playerName playerPosition = Player playerName newPlayerHealth playerPosition

initGameLoop :: RNG -> String -> GameState
initGameLoop rng playerName =
    let (playerCoords, newDungeon) = dungeonGenerator rng in
    GameState newDungeon (newPlayer playerName playerCoords)

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
