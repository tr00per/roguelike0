module Dungeon.Generator.Empty where

import           Data.Matrix
import           Model
import           System.Random (RandomGen)

newDungeon :: RandomGen r => Coords -> r -> (Coords, Board)
newDungeon bounds _ = (heroPos, matrix maxY maxX filler)
    where
        maxX = getX bounds
        maxY = getY bounds
        heroPos = Coords (maxX `div` 2) (maxY `div` 2)
        filler (y, x) = if x == getX heroPos && y == getY heroPos
                        then [Hero, Floor]
                        else [Floor]
