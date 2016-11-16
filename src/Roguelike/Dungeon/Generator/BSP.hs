{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Roguelike.Dungeon.Generator.BSP (newDungeon) where

import           Control.Eff
import           Control.Eff.State.Lazy
import           Roguelike.Model
import           Roguelike.Random

type NewDungeon = (Coords, Board)

data BSP = Leaf SingleCoord SingleCoord SingleCoord SingleCoord
         | Split BSP BSP

split :: (Member (State RNG) e) =>  BSP -> Eff e BSP
split = undefined

newDungeon :: Coords -> RNG -> NewDungeon
newDungeon bounds rng = run $ evalState rng (generator maxX maxY)
    where
        maxX = getX bounds
        maxY = getY bounds

generator :: (Member (State RNG) e) => SingleCoord -> SingleCoord -> Eff e NewDungeon
generator _ _ = undefined
