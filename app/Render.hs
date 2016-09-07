module Render
    ( render
    ) where

import           Board

render :: GameState -> String
render = unlines . map (map renderPiece) . board

renderPiece :: Piece -> Char
renderPiece Blank = ' '
renderPiece Wall = '#'
renderPiece Floor = '.'
renderPiece Hero = '@'
