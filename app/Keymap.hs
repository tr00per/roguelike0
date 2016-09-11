module Keymap
    ( kmap

    -- re-export from Actions
    , Action
    ) where

import           Actions
import           UI.NCurses (Event (..), Key (..))

kmap :: Event -> Action
kmap (EventCharacter 'Q') = Meta Quit
kmap (EventSpecialKey KeyUpArrow) = Go North
kmap (EventSpecialKey KeyRightArrow) = Go East
kmap (EventSpecialKey KeyDownArrow) = Go South
kmap (EventSpecialKey KeyLeftArrow) = Go West
kmap _ = Meta NoAction
