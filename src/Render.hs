module Render
    ( render
    ) where

import           Board
-- import qualified UI.HSCurses.Curses       as Curses
-- import qualified UI.HSCurses.CursesHelper as CursesH

render :: Board -> String
render = concatMap (map renderPiece)

renderPiece :: Piece -> Char
renderPiece Blank = ' '
renderPiece Wall = '#'
renderPiece Floor = '.'
renderPiece Player = '@'
