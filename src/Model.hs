module Model where

import           Data.Matrix (Matrix (..), toLists)

type SingleCoord = Int
type YXPair = (SingleCoord, SingleCoord)
data Coords = Coords
            { getX :: SingleCoord
            , getY :: SingleCoord
            } deriving (Eq, Show)

maxDungeonWidth, maxDungeonHeight :: SingleCoord
maxDungeonWidth = 79
maxDungeonHeight = 25

type Field = [Piece]
type Board = Matrix Field
type RenderBoard = [[Field]]

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

toYXPair :: Coords -> YXPair
toYXPair (Coords x y) = (y, x)
