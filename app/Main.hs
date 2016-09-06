module Main where

import           Control.Exception
import           Game                     (gameLoop, initGameLoop)
import           Keymap                   (kmap)
import           Render                   (render)
import qualified UI.HSCurses.Curses       as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

main :: IO ()
main = runInTerminal cursesLoop

runInTerminal :: (String -> IO ()) -> IO ()
runInTerminal run = do
    CursesH.start
    _ <- Curses.cursSet Curses.CursorInvisible
    run (render initGameLoop) `finally` CursesH.end

cursesLoop :: String -> IO ()
cursesLoop initialState = do
    Curses.wclear Curses.stdScr
    Curses.wAddStr Curses.stdScr initialState
    Curses.refresh
    key <- Curses.getCh
    let newScreen = render $ gameLoop $ kmap key
    cursesLoop newScreen
