module Dungeon where

import           Data.Matrix
import           Model

type CoordsTransform = Coords -> Coords

dungeonGenerator :: Coords -> Board
dungeonGenerator heroPos = matrix maxDungeonHeight maxDungeonWidth filler
    where
        filler (y, x) = if x == getX heroPos && y == getY heroPos
                        then Hero
                        else Floor

isWithinMap :: Board -> Coords -> Bool
isWithinMap currentBoard (Coords x y) = x >= 1 && x <= ncols currentBoard && y >= 1 && y <= nrows currentBoard

move :: CoordsTransform -> Coords -> Board -> (Coords, Board)
move trans coords b = let yxPair = toYXPair coords
                          srcValue = uncurry unsafeGet yxPair b
                          newPos = trans coords
                      in (newPos, unsafeSet srcValue (toYXPair newPos) $ unsafeSet Floor yxPair b)



north, south, west, east :: CoordsTransform
north coords = coords { getY = getY coords - 1 }
south coords = coords { getY = getY coords + 1 }
west coords = coords { getX = getX coords - 1 }
east coords = coords { getX = getX coords + 1 }

canPlayerMove :: CoordsTransform ->  Board -> Player -> Bool
canPlayerMove trans b p = let playerPos = position p in
                          canMoveFromTo b playerPos (trans playerPos)

canMoveFromTo :: Board -> Coords -> Coords -> Bool
canMoveFromTo b _ to = isWithinMap b to
