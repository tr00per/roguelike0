module Game.Keymap
    ( kmap

    -- re-export from Actions
    , Action
    ) where

import           Roguelike.Actions
import           UI.NCurses        (Event (..), Key (..))

kmap :: Maybe Event -> Action
kmap (Just (EventCharacter 'Q'))            = Meta Quit
kmap (Just (EventSpecialKey KeyUpArrow))    = Go North
kmap (Just (EventSpecialKey KeyDownArrow))  = Go South
kmap (Just (EventSpecialKey KeyLeftArrow))  = Go West
kmap (Just (EventSpecialKey KeyRightArrow)) = Go East
kmap _                                      = Meta NoAction
