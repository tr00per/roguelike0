module Dungeon where

import           Data.Matrix
import           Model


dungeonGenerator :: Coords -> Board
dungeonGenerator heroPos = matrix maxDungeonHeight maxDungeonWidth filler
    where
        filler (y, x) = if x == getX heroPos && y == getY heroPos
                        then Hero
                        else Floor

isWithinMap :: Board -> Coords -> Bool
isWithinMap currentBoard (Coords x y) = x >= 1 && x <= ncols currentBoard && y >= 1 && y <= ncols currentBoard

moveUp, moveDown, moveLeft, moveRight :: Coords -> Board -> Board
moveUp coords b = let x = getX coords
                      y = getY coords
                      srcValue = unsafeGet y x b
                  in unsafeSet srcValue (y - 1, x) $ unsafeSet Floor (y, x) b
moveDown coords b = let x = getX coords
                        y = getY coords
                        srcValue = unsafeGet y x b
                    in unsafeSet srcValue (y + 1, x) $ unsafeSet Floor (y, x) b
moveLeft coords b = let x = getX coords
                        y = getY coords
                        srcValue = unsafeGet y x b
                    in unsafeSet srcValue (y, x - 1) $ unsafeSet Floor (y, x) b
moveRight coords b = let x = getX coords
                         y = getY coords
                         srcValue = unsafeGet y x b
                     in unsafeSet srcValue (y, x + 1) $ unsafeSet Floor (y, x) b

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
