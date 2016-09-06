module Keymap
    ( kmap
    ) where

import           Actions            (Action (..), Control (..), Direction (..))
import           UI.HSCurses.Curses (Key (..))

kmap :: Key -> Action
kmap (KeyChar 'Q') = Meta Quit
kmap KeyUp = Go North
kmap KeyRight = Go East
kmap KeyDown = Go South
kmap KeyLeft = Go West
kmap _ = Meta NoAction
