{-# LANGUAGE GeneralizedNewtypeDeriving, Rank2Types #-}
module Main where

import Emulator
import Emulator.IO
import Instruction
import Memory
import Control.Monad.Trans (liftIO)

main :: IO ()
main = do
    x <- getLine
    runIOEmulator $ do
        performOperation $ BasicInstruction SET (OpRegister A) (OpLiteral 65)
        performOperation $ BasicInstruction SET OpPush (OpRegister A)
        liftIO $ print x
    return ()