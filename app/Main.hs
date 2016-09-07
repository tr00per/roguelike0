module Main where

import           Board      (GameState, RoundResult (..))
import           Game       (initGameLoop, runGameLoop)
import           Keymap     (kmap)
import           Render     (render)
import qualified UI.NCurses as Curses

main :: IO ()
main = runInTerminal

runInTerminal :: IO ()
runInTerminal = Curses.runCurses $ do
    _ <- Curses.setCursorMode Curses.CursorInvisible
    run $ initGameLoop "Fenter"

    where
        run :: GameState -> Curses.Curses ()
        run gameState = do
            w <- Curses.defaultWindow
            Curses.updateWindow w $ do
                Curses.clear
                Curses.drawString (render gameState)
            Curses.render
            (Just ev) <- Curses.getEvent w Nothing
            next ev gameState $ runGameLoop (kmap ev) gameState

        next :: Curses.Event -> GameState -> (RoundResult, GameState) -> Curses.Curses ()
        next Curses.EventResized oldState _ =
            run oldState
        next _ _ (GameOver, _) =
            return ()
        next _ _ (Continue, gs) =
            run gs
