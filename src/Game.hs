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
gameLoop (Go North) = do
    gs <- get
    let thePlayer = player gs
        theBoard = board gs
    when (canPlayerMoveUp theBoard thePlayer) $ modify $ \st ->
        st { board = moveUp (position thePlayer) (board st) }
    return Continue
gameLoop (Go South) = do
    gs <- get
    let thePlayer = player gs
        theBoard = board gs
    when (canPlayerMoveDown theBoard thePlayer) $ modify $ \st ->
        st { board = moveDown (position thePlayer) (board st) }
    return Continue
gameLoop (Go West) = do
    gs <- get
    let thePlayer = player gs
        theBoard = board gs
    when (canPlayerMoveLeft theBoard thePlayer) $ modify $ \st ->
        st { board = moveLeft (position thePlayer) (board st) }
    return Continue
gameLoop (Go East) = do
    gs <- get
    let thePlayer = player gs
        theBoard = board gs
    when (canPlayerMoveRight theBoard thePlayer) $ modify $ \st ->
        st { board = moveRight (position thePlayer) (board st) }
    return Continue

gameLoop (Meta Quit) =
    return GameOver
gameLoop _ =
    return Continue

canPlayerMoveUp, canPlayerMoveDown, canPlayerMoveLeft, canPlayerMoveRight :: Board -> Player -> Bool
canPlayerMoveUp b p = let playerPos = position p in
                      canMoveFromTo b playerPos playerPos { getY = getY playerPos - 1 }

canPlayerMoveDown b p = let playerPos = position p in
                        canMoveFromTo b playerPos playerPos { getY = getY playerPos + 1 }

canPlayerMoveLeft b p = let playerPos = position p in
                        canMoveFromTo b playerPos playerPos { getX = getX playerPos - 1 }

canPlayerMoveRight b p = let playerPos = position p in
                         canMoveFromTo b playerPos playerPos { getX = getX playerPos + 1 }

canMoveFromTo :: Board -> Coords -> Coords -> Bool
canMoveFromTo b _ to = isWithinMap b to
