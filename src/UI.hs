module UI
    ( appMain
    ) where

import           Control.Exception
import qualified UI.HSCurses.Curses       as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

appMain :: IO ()
appMain = runInTerminal cursesLoop

runInTerminal :: IO () -> IO ()
runInTerminal run = do
    CursesH.start
    _ <- Curses.cursSet Curses.CursorInvisible
    run `finally` CursesH.end

cursesLoop :: IO ()
cursesLoop = do
    Curses.refresh
    key <- Curses.getCh
    case key of
        Curses.KeyBackspace -> return ()
        Curses.KeyChar ch   -> Curses.wAddStr Curses.stdScr [ch] >> cursesLoop
        _                   -> Curses.wAddStr Curses.stdScr "Not a character (Backspace to exit)" >> cursesLoop
