module Dungeon.Generator.Empty where

import           Data.Matrix
import           Model
import           System.Random (RandomGen)

newDungeon :: RandomGen r => r -> (Coords, Board)
newDungeon _ = (heroPos, matrix maxDungeonHeight maxDungeonWidth filler)
    where
        heroPos = Coords (maxDungeonWidth `div` 2) (maxDungeonHeight `div` 2)
        filler (y, x) = if x == getX heroPos && y == getY heroPos
                        then [Hero, Floor]
                        else [Floor]
