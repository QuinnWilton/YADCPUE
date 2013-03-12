{-# LANGUAGE GeneralizedNewtypeDeriving, Rank2Types #-}
module Emulator where

import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.Trans
import Data.Word (Word16)
import Memory
import Instruction

class Monad m => Emulator m where
	load  :: Address -> m Word16
	store :: Address -> Word16 -> m ()

newtype STEmulator s a = STEmulator (ReaderT (Memory s) (ST s) a)
	deriving (Monad)

instance Emulator (STEmulator s) where
	load address = STEmulator $ do
		memory <- ask
		lift $ Memory.load memory address
	store address word = STEmulator $ do
		memory <- ask
		lift $ Memory.store memory address word

runSTEmulator :: (forall s. STEmulator s a) -> a
runSTEmulator emulator =
	runST $ run emulator
	where
		run :: STEmulator s a -> ST s a
		run (STEmulator reader) = do
			memory <- Memory.new
			runReaderT reader memory