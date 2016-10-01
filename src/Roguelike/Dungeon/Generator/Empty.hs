module Roguelike.Dungeon.Generator.Empty (newDungeon) where

import           Roguelike.Model
import           Roguelike.Random

newDungeon :: Coords -> RNG -> (Coords, Board)
newDungeon bounds _ = (heroPos, mkBoard bounds filler)
    where
        maxX = getX bounds
        maxY = getY bounds
        heroPos = Coords (maxX `div` 2) (maxY `div` 2)
        generate y x = if x == getX heroPos && y == getY heroPos
                       then [Hero, Floor]
                       else [Floor]
        filler :: [Field]
        filler = do
            y <- [0..maxY-1]
            x <- [0..maxX-1]
            return $ generate y x
