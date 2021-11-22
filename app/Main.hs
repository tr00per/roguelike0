{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Eff
import           Control.Eff.Reader.Lazy
import           Control.Eff.State.Lazy
import           Game.Keymap             (kmap)
import           Game.Render             (Palette, initPalette, render)
import           Roguelike.Game          (GameState, RoundResult (..), gameLoop,
                                          initGameLoop)
import           Roguelike.Random        (mkRNG)
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
        (rr, gs) <- runLift . runReader palette . runState initialGameState $ runGame
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
                (Just (Curses.EventCharacter _))  -> return ()
                (Just (Curses.EventSpecialKey _)) -> return ()
                _                                 -> waitForAnyKey w

runGame :: ( Lifted Curses.Curses e
           , Member (State GameState) e
           , Member (Reader Palette) e
           ) => Eff e RoundResult
runGame = do
    w <- lift Curses.defaultWindow
    palette <- ask
    gameState <- get
    lift $ Curses.updateWindow w $ do
        Curses.clear
        render palette gameState
    lift Curses.render
    ev <- lift $ Curses.getEvent w Nothing
    next ev gameLoop
    where
        next (Just Curses.EventResized) _ =
            runGame
        next ev step = do
            roundResult <- step (kmap ev)
            case roundResult of
                GameOver ->
                    return GameOver
                Continue ->
                    runGame
