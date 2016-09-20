
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Roguelike.Random where

import           Control.Eff
import           Control.Eff.State.Lazy
import           Roguelike.Model        (SingleCoord)
import           System.Random          (StdGen, getStdGen, random, randomR)


newtype RNG = RNG StdGen

mkRNG :: IO RNG
mkRNG = RNG <$> getStdGen

roll :: (Member (State RNG) e) => Eff e SingleCoord
roll = do
    (RNG rng) <- get
    let (result, rng') = random rng
    put (RNG rng')
    return result

rollR :: (Member (State RNG) e) => SingleCoord -> SingleCoord -> Eff e SingleCoord
rollR lo hi = do
    (RNG rng) <- get
    let (result, rng') = randomR (lo, hi) rng
    put (RNG rng')
    return result

rollFor :: (Member (State RNG) e) => SingleCoord -> Eff e SingleCoord
rollFor limit = rollR 0 (limit-1)
