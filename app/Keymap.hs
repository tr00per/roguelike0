module Keymap
    ( kmap
    ) where

import           Actions    (Action (..), Control (..), Direction (..))
import           UI.NCurses (Event (..), Key (..))

kmap :: Event -> Action
kmap (EventCharacter 'Q') = Meta Quit
kmap (EventSpecialKey KeyUpArrow) = Go North
kmap (EventSpecialKey KeyRightArrow) = Go East
kmap (EventSpecialKey KeyDownArrow) = Go South
kmap (EventSpecialKey KeyLeftArrow) = Go West
kmap _ = Meta NoAction
