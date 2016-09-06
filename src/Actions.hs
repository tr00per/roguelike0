module Actions
    ( Action (..)
    , Direction (..)
    , Control (..)
    ) where

data Action = Meta Control
            | Go Direction
            deriving (Eq, Show)

data Direction = North
               | East
               | South
               | West
               deriving (Eq, Show)

data Control = Quit
             | NoAction
             deriving (Eq, Show)
