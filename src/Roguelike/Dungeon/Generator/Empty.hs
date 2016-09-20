module Roguelike.Dungeon.Generator.Empty (newDungeon) where

import           Data.Matrix
import           Roguelike.Model
import           Roguelike.Random

newDungeon :: Coords -> RNG -> (Coords, Board)
newDungeon bounds _ = (heroPos, matrix maxY maxX filler)
    where
        maxX = getX bounds
        maxY = getY bounds
        heroPos = Coords (maxX `div` 2) (maxY `div` 2)
        filler (y, x) = if x == getX heroPos && y == getY heroPos
                        then [Hero, Floor]
                        else [Floor]
