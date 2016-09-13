module Model where

import           Data.Matrix (Matrix (..), toLists)

data Coords = Coords
            { getX :: Int
            , getY :: Int
            } deriving (Eq, Show)

maxDungeonWidth, maxDungeonHeight :: Int
maxDungeonWidth = 79
maxDungeonHeight = 21

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
