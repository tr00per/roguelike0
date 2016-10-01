module Roguelike.Model.Defs where

data Piece = Blank
           | Wall
           | Floor
           | Hero
           deriving (Eq, Show)

data RoundResult = GameOver | Continue
                 deriving (Eq, Show)

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
type RenderBoard = [[Field]]
type YXPair = (SingleCoord, SingleCoord)
