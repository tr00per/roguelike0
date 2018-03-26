{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Roguelike.Dungeon.Generator.BSP (newDungeon) where

import           Control.Eff
import           Control.Eff.State.Lazy
import           Roguelike.Model
import           Roguelike.Random

minimumSize, maxSpread :: SingleCoord
minimumSize = 6
maxSpread = minimumSize `div` 2

type NewDungeon = (Coords, Board)

data BSP = Leaf { x0::SingleCoord
                , x1::SingleCoord
                , y0::SingleCoord
                , y1::SingleCoord
                }
         | Split BSP BSP

newDungeon :: Coords -> RNG -> NewDungeon
newDungeon bounds rng = run $ evalState rng (generator maxX maxY)
    where
        maxX = getX bounds
        maxY = getY bounds

generator :: (Member (State RNG) e) => SingleCoord -> SingleCoord -> Eff e NewDungeon
generator maxX maxY = createTree maxX maxY >>= addCorridors >>= toNewDungeon maxX maxY

split :: (Member (State RNG) e) =>  BSP -> Eff e BSP
split leaf @ Leaf {} = do
    let sizeX = x1 leaf - x0 leaf
        sizeY = y1 leaf - y0 leaf
    if sizeX >= minimumSize || sizeY >= minimumSize
    then do
         let halfX = (x0 leaf + x1 leaf) `div` 2
             halfY = (y0 leaf + y1 leaf) `div` 2
         if sizeX > sizeY
            then do
                pivotX <- rollR (halfX-maxSpread) (halfX+maxSpread)
                left <- split $ leaf {x1=pivotX}
                right <- split $ leaf {x0=pivotX}
                return $ Split left right
            else do
                pivotY <- rollR (halfY-maxSpread) (halfY+maxSpread)
                up <- split $ leaf {y1=pivotY}
                down <- split $ leaf {y0=pivotY}
                return $ Split up down
    else return leaf
split _ = error "Can't split what's already splitted"

createTree :: (Member (State RNG) e) => SingleCoord -> SingleCoord -> Eff e BSP
createTree maxX maxY = split (Leaf 0 maxX 0 maxY)

addCorridors :: BSP -> Eff e BSP
addCorridors = return

toNewDungeon :: SingleCoord -> SingleCoord -> BSP -> Eff e NewDungeon
toNewDungeon maxX maxY bsp = do
    let rooms = toRooms bsp
        walls = concatMap toWalls rooms
        heroPos = Coords 1 1
        withFloor = emptyBoard maxX maxY
        withWalls = populateFields Wall walls withFloor
        withHero = populateFields Hero [heroPos] withWalls
    return (heroPos, withHero)

emptyBoard :: SingleCoord -> SingleCoord -> Board
emptyBoard maxX maxY = mkBoard (Coords maxX maxY) [[Floor] | y <- [0..maxY-1], x <- [0..maxX-1]]

toRooms :: BSP -> [BSP]
toRooms leaf @ Leaf{}           = [leaf]
toRooms (Split branch0 branch1) = toRooms branch0 ++ toRooms branch1

toWalls :: BSP -> [Coords]
toWalls leaf @ Leaf{}           = [Coords (x0 leaf) (y0 leaf), Coords (x1 leaf) (y1 leaf)]
toWalls (Split branch0 branch1) = toWalls branch0 ++ toWalls branch1
