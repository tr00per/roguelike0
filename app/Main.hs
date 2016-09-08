module Main where

import           Board      (GameState, RoundResult (..))
import           Game       (initGameLoop, runGameLoop)
import           Keymap     (kmap)
import           Render     (Palette, initPalette, render)
import qualified UI.NCurses as Curses

main :: IO ()
main = runInTerminal

runInTerminal :: IO ()
runInTerminal = Curses.runCurses $ do
    _ <- Curses.setCursorMode Curses.CursorInvisible
    palette <- initPalette
    run palette $ initGameLoop "Fenter"

    where
        run :: Palette -> GameState -> Curses.Curses ()
        run palette gameState = do
            w <- Curses.defaultWindow
            Curses.updateWindow w $ do
                Curses.clear
                render palette gameState
            Curses.render
            (Just ev) <- Curses.getEvent w Nothing
            next ev palette gameState $ runGameLoop (kmap ev) gameState

        next :: Curses.Event -> Palette -> GameState -> (RoundResult, GameState) -> Curses.Curses ()
        next Curses.EventResized palette oldState _ =
            run palette oldState
        next _ _ _ (GameOver, _) =
            return ()
        next _ palette _ (Continue, gs) =
            run palette gs
