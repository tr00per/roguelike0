module Main where

import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH
import Control.Exception

main :: IO ()
main = do
    CursesH.start
    _ <- Curses.cursSet Curses.CursorInvisible
    run `finally` CursesH.end

run :: IO ()
run = do
    Curses.refresh
    key <- Curses.getCh
    case key of
        Curses.KeyBackspace -> return ()
        Curses.KeyChar ch   -> Curses.wAddStr Curses.stdScr [ch] >> run
        _                   -> Curses.wAddStr Curses.stdScr "Not a character (Backspace to exit)" >> run
