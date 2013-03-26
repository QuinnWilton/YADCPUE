module Emulator.Monad where

import Data.Word (Word16)
import Memory (Address)

class Monad m => Emulator m where
    load  :: Address -> m Word16
    store :: Address -> Word16 -> m ()