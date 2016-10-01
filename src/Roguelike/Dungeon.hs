module Roguelike.Dungeon where

import qualified Roguelike.Dungeon.Generator.Empty as Gen
import           Roguelike.Model
import           Roguelike.Random                  (RNG)

type CoordsTransform = Coords -> Coords

dungeonGenerator :: RNG -> (Coords, Board)
dungeonGenerator = Gen.newDungeon maxDungeonSize

move :: CoordsTransform -> Coords -> Board -> (Coords, Board)
move trans oldPos b = let newPos = trans oldPos
                      in (newPos, moveTop oldPos newPos b)

north, south, west, east :: CoordsTransform
north coords = coords { getY = getY coords - 1 }
south coords = coords { getY = getY coords + 1 }
west coords = coords { getX = getX coords - 1 }
east coords = coords { getX = getX coords + 1 }

canPlayerMove :: CoordsTransform ->  Board -> Player -> Bool
canPlayerMove trans b p = let playerPos = position p
                          in canMoveFromTo b playerPos (trans playerPos)

canMoveFromTo :: Board -> Coords -> Coords -> Bool
canMoveFromTo b _ to = isWithinBoard b to
