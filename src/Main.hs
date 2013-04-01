{-# LANGUAGE GeneralizedNewtypeDeriving, Rank2Types #-}
module Main where

import Emulator
import Emulator.IO
import Instruction
import Memory
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString as BS

main :: IO ()
main = do
    x <- BS.readFile "blargh"
    runIOEmulator $ do
        loadProgram x
        printRange performIO 0x0 0x64

    where performIO = liftIO . putStrLn