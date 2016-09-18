module Dungeon where

import           Data.Matrix
import qualified Dungeon.Generator.Empty as Gen
import           Model
import           System.Random           (RandomGen)

type CoordsTransform = Coords -> Coords

dungeonGenerator :: RandomGen r => r -> (Coords, Board)
dungeonGenerator = Gen.newDungeon maxDungeonSize

isWithinMap :: Board -> Coords -> Bool
isWithinMap currentBoard (Coords x y) = x >= 1 && x <= ncols currentBoard && y >= 1 && y <= nrows currentBoard

move :: CoordsTransform -> Coords -> Board -> (Coords, Board)
move trans coords b = let newPos = trans coords
                          srcYXPair = toYXPair coords
                          destYXPair = toYXPair newPos
                          srcValue = uncurry unsafeGet srcYXPair b
                          destValue = uncurry unsafeGet destYXPair b
                      in (newPos, unsafeSet (head srcValue:destValue) destYXPair $ unsafeSet (tail srcValue) srcYXPair b)

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
