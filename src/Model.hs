module Model where

import           Data.Matrix (Matrix (..), toLists)

type SingleCoord = Int
data Coords = Coords
            { getX :: SingleCoord
            , getY :: SingleCoord
            } deriving (Eq, Show)

maxDungeonWidth, maxDungeonHeight :: SingleCoord
maxDungeonWidth = 79
maxDungeonHeight = 25

type Board = Matrix Piece
type RenderBoard = [[Piece]]

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

forRendering :: Board -> RenderBoard
forRendering = toLists

fromYXPair :: (SingleCoord, SingleCoord) -> Coords
fromYXPair (y, x) = Coords x y
