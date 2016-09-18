{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Dungeon.Generator.BSP (newDungeon, RNG (..)) where

import           Control.Eff
import           Control.Eff.State.Lazy
import           Model
import           System.Random

type NewDungeon = (Coords, Board)

newtype RNG = RNG StdGen

newDungeon :: Coords -> RNG -> NewDungeon
newDungeon bounds rng = run $ evalState rng (generator maxX maxY)
    where
        maxX = getX bounds
        maxY = getY bounds

generator :: (Member (State RNG) e) => SingleCoord -> SingleCoord -> Eff e NewDungeon
generator _ _ = undefined

-- roll :: (Member (State RNG) e) => Eff e Int
-- roll min max = do
