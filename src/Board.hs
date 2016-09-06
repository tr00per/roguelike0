module Board where

type Board = [[Piece]]

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
           { name   :: String
           , health :: Int
           } deriving (Eq, Show)
