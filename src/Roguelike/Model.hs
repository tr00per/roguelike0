module Roguelike.Model
    ( GameState (..)

    -- re-export of inner modules
    , module Roguelike.Model.Defs
    , module Roguelike.Model.Board
    ) where

import           Roguelike.Model.Board
import           Roguelike.Model.Defs

data GameState = GameState
               { board  :: Board
               , player :: Player
               } deriving (Eq, Show)
