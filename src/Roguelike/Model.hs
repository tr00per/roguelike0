{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Roguelike.Model where

import           Control.Monad.ST    (ST, runST)
import qualified Data.Matrix         as MI
import qualified Data.Matrix.Mutable as MM

data Piece = Blank
           | Wall
           | Floor
           | Hero
           deriving (Eq, Show)

data RoundResult = GameOver | Continue
                 deriving (Eq, Show)

data GameState = GameState
               { board  :: Board
               , player :: Player
               } deriving (Eq, Show)

data Player = Player
            { name     :: String
            , health   :: Int
            , position :: Coords
            } deriving (Eq, Show)

type SingleCoord = Int
data Coords = Coords
            { getX :: SingleCoord
            , getY :: SingleCoord
            } deriving (Eq, Show)

maxDungeonWidth, maxDungeonHeight :: SingleCoord
maxDungeonWidth = 79
maxDungeonHeight = 25

maxDungeonSize :: Coords
maxDungeonSize = Coords maxDungeonWidth maxDungeonHeight

type Field = [Piece]
newtype Board = Board (MI.Matrix Field)
    deriving (Show, Eq)
type RenderBoard = [[Field]]

type YXPair = (SingleCoord, SingleCoord)

class GameBoard g where
    mkBoard :: Coords -> [Field] -> g
    forRendering :: g -> RenderBoard
    boardMin, boardMax :: g -> Coords
    isWithinBoard :: g -> Coords -> Bool

instance GameBoard Board where
    mkBoard bounds filling = Board $ MI.matrix (getX bounds) filling
    forRendering (Board b) = MI.toLists b
    boardMin _ = Coords 0 0
    boardMax (Board b) = Coords (MI.cols b) (MI.rows b)
    isWithinBoard b (Coords x y) = let minX = getX $ boardMin b
                                       minY = getY $ boardMin b
                                       maxX = getX $ boardMax b
                                       maxY = getY $ boardMax b
        in x >= minX && x < maxX && y >= minY && y < maxY

toYXPair :: Coords -> YXPair
toYXPair (Coords x y) = (y, x)

mutateTop :: YXPair -> YXPair -> MI.Matrix Field -> ST a (MI.Matrix Field)
mutateTop src dest b = do
    mb <- MI.thaw b
    (this:rest) <- MM.unsafeRead mb src
    MM.unsafeWrite mb src rest
    others <- MM.unsafeRead mb dest
    MM.unsafeWrite mb dest (this:others)
    MI.unsafeFreeze mb

moveTop :: Coords -> Coords -> Board -> Board
moveTop oldPos newPos (Board b) =
    let srcYXPair = toYXPair oldPos
        destYXPair = toYXPair newPos
        b' = runST $ mutateTop srcYXPair destYXPair b
    in Board b'
