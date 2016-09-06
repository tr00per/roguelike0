module Board where

type Board = [[Piece]]

data Piece = Blank
           | Wall
           | Floor
           | Player
