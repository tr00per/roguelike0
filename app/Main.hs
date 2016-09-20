{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Main where

import           Control.Eff
import           Control.Eff.Lift
import           Control.Eff.Reader.Lazy
import           Control.Eff.State.Lazy
import           Roguelike.Game          (GameState, RoundResult (..), gameLoop,
                                          initGameLoop)
import           Roguelike.Keymap        (kmap)
import           Roguelike.Random        (mkRNG)
import           Roguelike.Render        (Palette, initPalette, render)
import qualified UI.NCurses              as Curses

main :: IO ()
main = runInTerminal

runInTerminal :: IO ()
runInTerminal = do
    rng <- mkRNG
    Curses.runCurses $ do
        _ <- Curses.setCursorMode Curses.CursorInvisible
        palette <- initPalette
        let initialGameState = initGameLoop rng "Fenter"
        (gs, rr) <- runLift $ runReader (runState initialGameState runGame) palette
        endScreen rr gs

endScreen :: RoundResult -> GameState -> Curses.Curses ()
endScreen _ _ = do
    w <- Curses.defaultWindow
    Curses.updateWindow w $ do
        Curses.clear
        Curses.drawString "The End"
    Curses.render
    waitForAnyKey w
    where
        waitForAnyKey w = do
            ev <- Curses.getEvent w Nothing
            case ev of
                (Just (Curses.EventCharacter _)) -> return ()
                (Just (Curses.EventSpecialKey _)) -> return ()
                _ -> waitForAnyKey w

runGame :: (SetMember Lift (Lift Curses.Curses) e, Member (State GameState) e, Member (Reader Palette) e) =>
    Eff e RoundResult
runGame = do
    w <- lift Curses.defaultWindow
    palette <- ask
    gameState <- get
    lift $ Curses.updateWindow w $ do
        Curses.clear
        render palette gameState
    lift Curses.render
    (Just ev) <- lift $ Curses.getEvent w Nothing
    next ev gameLoop
    where
        next Curses.EventResized _ =
            runGame
        next ev step = do
            roundResult <- step (kmap ev)
            case roundResult of
                GameOver ->
                    return GameOver
                Continue ->
                    runGame
