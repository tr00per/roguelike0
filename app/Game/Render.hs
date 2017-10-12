module Game.Render
    ( initPalette
    , render
    , Palette
    ) where

import           Data.List       (intercalate)
import           Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import           Roguelike.Model
import           UI.NCurses      (Attribute (..), Color (..), ColorID, Curses,
                                  Glyph (..), Update, drawGlyph, newColorID)

newtype Palette = Palette (Map.Map String ColorID)

render :: Palette -> GameState -> Update ()
render palette = mapM_ drawGlyph . colorCont . map (map colorPiece) . forRendering . board
    where colorPiece = renderPiece palette
          colorCont = continuous palette

continuous ::  Palette -> [[Glyph]] -> [Glyph]
continuous (Palette palette) = intercalate [sep]
    where sep = Glyph '\n' [AttributeColor (palette ! "default")]

renderPiece ::  Palette -> Field -> Glyph
renderPiece (Palette palette) [] =
    Glyph ' ' [AttributeColor (palette ! "default")]
renderPiece (Palette palette) (Blank:_) =
    Glyph ' ' [AttributeColor (palette ! "default")]
renderPiece (Palette palette) (Wall:_) =
    Glyph '#' [AttributeColor (palette ! "white")]
renderPiece (Palette palette) (Floor:_) =
    Glyph '.' [AttributeColor (palette ! "default")]
renderPiece (Palette palette) (Hero:_) =
    Glyph '@' [AttributeColor (palette ! "green")]

initPalette :: Curses Palette
initPalette = do
    colorIds <- sequence [ newColorID ColorDefault ColorDefault 1
                         , newColorID ColorWhite ColorBlack 2
                         , newColorID ColorGreen ColorBlack 3
                         ]
    let makePalette = Palette . Map.fromList . zip ["default", "white", "green"]
    return $ makePalette colorIds
