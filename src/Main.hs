{-# LANGUAGE GeneralizedNewtypeDeriving, Rank2Types #-}
module Main where

import Emulator
import Emulator.IO
import Instruction
import Memory
import Control.Monad.Trans (liftIO)

main :: IO ()
main = do
    runIOEmulator $ do
        performOperation $ BasicInstruction SET OpPush (OpLiteral 10)
        performOperation $ BasicInstruction SET OpPush (OpLiteral 65532)
        printRegisters performIO
        printRange performIO 0xFFF0 0xFFFF
        performOperation $ BasicInstruction SET (OpRegister A) OpPop
        performOperation $ BasicInstruction SET (OpRegister B) OpPop
        performOperation $ BasicInstruction SET (OpRegister C) OpPop
        printRegisters performIO
        printRange performIO 0xFFF0 0xFFFF
    where performIO = liftIO . putStrLn