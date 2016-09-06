module Main where

import           Board                    (GameState, RoundResult (..))
import           Control.Exception        (finally)
import           Game                     (initGameLoop, runGameLoop)
import           Keymap                   (kmap)
import           Render                   (render)
import qualified UI.HSCurses.Curses       as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

main :: IO ()
main = runInTerminal

runInTerminal :: IO ()
runInTerminal = do
    CursesH.start
    _ <- Curses.cursSet Curses.CursorInvisible
    run (initGameLoop "Fenter") `finally` CursesH.end
    where
        run :: GameState -> IO ()
        run gameState = do
            Curses.wclear Curses.stdScr
            Curses.wAddStr Curses.stdScr (render gameState)
            Curses.refresh
            key <- Curses.getCh
            next $ runGameLoop (kmap key) gameState

        next :: (RoundResult, GameState) -> IO ()
        next (GameOver, _) = return ()
        next (Continue, gs) = run gs
