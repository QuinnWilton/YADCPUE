{-# LANGUAGE GeneralizedNewtypeDeriving, Rank2Types #-}
module Emulator.ST where

import Emulator.Monad
import Memory

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (lift)

newtype STEmulator s a = STEmulator (ReaderT (Memory s) (ST s) a)
    deriving (Monad)

instance Emulator (STEmulator s) where
    load address = STEmulator $ do
        memory <- ask
        lift $ Memory.read memory address
    store address word = STEmulator $ do
        memory <- ask
        lift $ Memory.write memory address word

runSTEmulator :: (forall s. STEmulator s a) -> a
runSTEmulator emulator =
    runST $ run emulator
    where
        run :: STEmulator s a -> ST s a
        run (STEmulator reader) = do
            memory <- Memory.new
            runReaderT reader memory