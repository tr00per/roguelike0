module Render
    ( initPalette
    , render
    , Palette
    ) where

import           Board
import           Data.List  (intercalate)
import           UI.NCurses (Attribute (..), Color (..), ColorID, Curses,
                             Glyph (..), Update, drawGlyph, newColorID)

type Palette = [ColorID]

render :: Palette -> GameState -> Update ()
render palette = mapM_ drawGlyph . colorCont . map (map colorPiece) . board
    where colorPiece = renderPiece palette
          colorCont = continuous palette

continuous ::  Palette -> [[Glyph]] -> [Glyph]
continuous palette = intercalate [sep]
    where sep = Glyph '\n' [AttributeColor (palette !! 0)]

renderPiece ::  Palette -> Piece -> Glyph
renderPiece palette Blank = Glyph ' ' [AttributeColor (palette !! 0)]
renderPiece palette Wall = Glyph '#' [AttributeColor (palette !! 1)]
renderPiece palette Floor = Glyph '.' [AttributeColor (palette !! 0)]
renderPiece palette Hero = Glyph '@' [AttributeColor (palette !! 2)]

initPalette :: Curses Palette
initPalette = sequence [ newColorID ColorDefault ColorDefault 1
                       , newColorID ColorWhite ColorBlack 2
                       , newColorID ColorGreen ColorBlack 3
                       ]
