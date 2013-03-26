{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}
module Emulator where

import Instruction
import Emulator.Monad
import Memory

import Control.Monad (unless, liftM)
import Data.Bits
import Data.Word (Word16, Word32)
import Data.Int (Int16)
import Data.ByteString (ByteString)
import Data.ByteString as B hiding (ByteString)
import Debug.Trace

-- Note:  Todo: Implement failure case for Instruction Operation (Literal x), Operand
loadAddress :: Emulator m => Operand -> m Address
loadAddress (OpRegister r)         = return $ Register r
loadAddress (OpRegisterPointer p)  = liftM Word . load $ Register p
loadAddress (OpRegisterNextWord w) =
    do
        incrementPC
        x <- load $ Register PC
        y <- load $ Word x
        z <- load $ Register w
        return . Word $ z + y

loadAddress OpPush                 =
    do
        x <- load $ Register SP
        store (Register SP) $ x - 1
        return . Word $ x - 1

loadAddress OpPeek            = liftM Word . load $ Register SP
loadAddress OpPick            =
    do
        incrementPC
        x <- load $ Register PC
        y <- load $ Register SP
        z <- load $ Word x
        return . Word $ y + z

loadAddress OpSP              = return $ Register SP
loadAddress OpPC              = return $ Register PC
loadAddress OpEX              = return $ Register EX
loadAddress OpNextWordPointer =
    do
        incrementPC
        x <- load $ Register PC
        y <- load $ Word x
        return . Word $ y

loadAddress OpNextWord        =
    do
        incrementPC
        x <- load $ Register PC
        return $ Word x

loadAddress (OpLiteral _)     = return $ Word 1  -- Have not defined silent failure case for literal address writing

loadValue :: Emulator m => Operand -> m Word16
loadValue (OpRegister r)         = load $ Register r
loadValue (OpRegisterPointer p)  = load (Register p) >>= load . Word
loadValue (OpRegisterNextWord w) =
    do
        incrementPC
        x <- load $ Register PC
        y <- load $ Word x
        z <- load $ Register w
        load . Word $ z + y

loadValue OpPop                  =
    do
        x <- load $ Register SP
        y <- load $ Word x
        store (Word x) 0
        store (Register SP) $ x + 1
        return y

loadValue OpPeek            = load (Register SP) >>= load . Word
loadValue OpPick            =
    do
        incrementPC
        x <- load $ Register PC
        y <- load $ Register SP
        z <- load $ Word x
        load . Word $ y + z

loadValue OpSP              = load $ Register SP
loadValue OpPC              = load $ Register PC
loadValue OpEX              = load $ Register EX
loadValue OpNextWordPointer = incrementPC >> load (Register PC) >>= load . Word >>= load . Word
loadValue OpNextWord        = incrementPC >> load (Register PC) >>= load . Word
loadValue (OpLiteral w)     = return w

i32 :: Integral a => a -> Int
i32 r = fromIntegral r :: Int

i16 :: Integral a => a -> Int16
i16 r = fromIntegral r :: Int16

incrementPC :: Emulator m => m ()
incrementPC = do
                x <- load $ Register PC
                store (Register PC) $ x + 1

hwQuantity :: Word16
hwQuantity = 0                  -- Work with this later, when we actually want to think about hardware

hwInfo :: Word16 -> (Word32, Word16, Word32)
hwInfo a = (0,0,0)              -- Something to keep the emulator happy?

hwSendINT :: Emulator m => Word16 -> m ()
hwSendINT a = return ()         -- More emulator placebos

performOperation :: Emulator m => Instruction -> m ()
performOperation (BasicInstruction SET b c) =
    do 
        x <- loadAddress b
        y <- loadValue c
        store x y

performOperation (BasicInstruction ADD b c) =
    do
        x <- loadValue b
        y <- loadValue c
        z <- loadAddress b
        store z $ x + y
        store (Register EX) $
            if fromIntegral x + fromIntegral y > (0xFFFF :: Int) then 1
            else 0

performOperation (BasicInstruction SUB b c) =
    do
        x <- loadValue b
        y <- loadValue c
        z <- loadAddress b
        store z $ x - y
        store (Register EX) $
            if fromIntegral x - fromIntegral y < (0 :: Int) then 0xFFFF
            else 0

performOperation (BasicInstruction MUL b c) =
    do
        x <- loadValue b
        y <- loadValue c
        z <- loadAddress b
        store z $ x * y
        store (Register EX) $
            fromIntegral (shiftR (fromIntegral x * fromIntegral y)
            16 .&. 0xFFFF :: Word32)

performOperation (BasicInstruction MLI b c) =
    do
        x <- loadValue b
        y <- loadValue c
        z <- loadAddress b
        store z $ fromIntegral $ i16 x * i16 x
        store (Register EX) $
            fromIntegral $ shiftR (i32 x * i32 y) 16 .&. 0xFFFF

performOperation (BasicInstruction DIV b c) =
    do
        x <- loadValue b
        y <- loadValue c
        z <- loadAddress b
        if y == 0 then do
            store z 0
            store (Register EX) 0
        else do
            store z $ div x y
            store (Register EX) $
                fromIntegral (div (shiftL (fromIntegral x) 16) $
                fromIntegral y .&. 0xFFFF :: Word32)

performOperation (BasicInstruction DVI b c) =
    do
        x <- loadValue b
        y <- loadValue c
        z <- loadAddress b
        if y == 0 then do
            store z 0
            store (Register EX) 0
        else do
            store z $ fromIntegral $ div (i16 x) (i16 y)
            store (Register EX) $
                fromIntegral $ div (shiftL (i32 x) 16) (i32 y) .&. 0xFFFF

performOperation (BasicInstruction MOD b c) =
    do
        x <- loadValue b
        y <- loadValue c
        z <- loadAddress b
        store z $ if y == 0 then 0 else mod x y

performOperation (BasicInstruction MDI b c) =
    do
        x <- loadValue b
        y <- loadValue c
        z <- loadAddress b
        store z $ 
            if y == 0 then 0 
            else fromIntegral $ abs (i32 x) `mod` i32 y

performOperation (BasicInstruction AND b c) =
    do
        x <- loadValue b
        y <- loadValue c
        z <- loadAddress b
        store z $ x .&. y

performOperation (BasicInstruction BOR b c) =
    do
        x <- loadValue b
        y <- loadValue c
        z <- loadAddress b
        store z $ x .|. y

performOperation (BasicInstruction XOR b c) =
    do
        x <- loadValue b
        y <- loadValue c
        z <- loadAddress b
        store z $ xor x y

performOperation (BasicInstruction SHR b c) =
	do
        x <- loadValue b
        y <- loadValue c
        z <- loadAddress b
        let (x', y') = (fromIntegral x :: Word32, fromIntegral y)
        store z $ shiftR x y'
        store (Register EX) $
            fromIntegral (shiftR (shiftL x' 16) y' .&. 0xFFFF)

performOperation (BasicInstruction ASR b c) =
    do
        x <- loadValue b
        y <- loadValue c
        z <- loadAddress b
        let (x', y') = (fromIntegral x :: Word32, fromIntegral y)
        store z $ fromIntegral $ shiftR (i16 x) y'
        store (Register EX) $ 
            fromIntegral (shiftR (i32 (shiftL x' 16)) y' .&. 0xFFFF)

performOperation (BasicInstruction SHL b c) =
    do
        x <- loadValue b
        y <- loadValue c
        z <- loadAddress b
        let (x', y') = (fromIntegral x :: Word32, fromIntegral y)
        store z $ shiftL x $ fromIntegral y
        store (Register EX) $
            fromIntegral (shiftR (shiftL x' y') 16 .&. 0xFFFF)

performOperation (BasicInstruction IFB b c) =
    do
        x <- loadValue b
        y <- loadValue c
        unless (x .&. y /= 0) incrementPC

performOperation (BasicInstruction IFC b c) =
    do
        x <- loadValue b
        y <- loadValue c
        unless (x .&. y == 0) incrementPC

performOperation (BasicInstruction IFE b c) =
    do
        x <- loadValue b
        y <- loadValue c
        unless (x == y) incrementPC

performOperation (BasicInstruction IFN b c) =
    do
        x <- loadValue b
        y <- loadValue c
        unless (x /= y) incrementPC

performOperation (BasicInstruction IFG b c) =
    do
        x <- loadValue b
        y <- loadValue c
        unless (x > y) incrementPC

performOperation (BasicInstruction IFA b c) =
    do
        x <- loadValue b
        y <- loadValue c
        unless (i16 x > i16 y) incrementPC

performOperation (BasicInstruction IFL b c) =
    do
        x <- loadValue b
        y <- loadValue c
        unless (x < y) incrementPC

performOperation (BasicInstruction IFU b c) =
    do
        x <- loadValue b
        y <- loadValue c
        unless (i16 x < i16 y) incrementPC

performOperation (BasicInstruction ADX b c) =
    do
        x <- loadValue b
        y <- loadValue c
        z <- loadAddress b
        火 <- load $ Register EX
        store z $ x + y + 火
        store (Register EX) $
            if fromIntegral x + fromIntegral y + fromIntegral 火 > (0xFFFF :: Int) then 1
            else 0

performOperation (BasicInstruction SBX b c) =
    do
        x <- loadValue b
        y <- loadValue c
        z <- loadAddress b
        水 <- load $ Register EX
        store z $ x - y + 水
        let check = fromIntegral x - fromIntegral y + fromIntegral 水
        store (Register EX) $ 
            if check < 0 then
                0xFFFF
            else if check > 0xFFFF then
                1
            else
                0

performOperation (BasicInstruction STI b c) =
    do
        y <- loadValue c
        i <- load $ Register I
        j <- load $ Register J
        z <- loadAddress b
        store z y
        store (Register I) $ i + 1
        store (Register J) $ j + 1

performOperation (BasicInstruction STD b c) =
    do
        y <- loadValue c
        i <- load $ Register I
        j <- load $ Register J
        z <- loadAddress b
        store z y
        store (Register I) $ i - 1
        store (Register J) $ j - 1

performOperation (SpecialInstruction JSR b) =
    do
        x <- loadValue b
        y <- load $ Register PC
        z <- loadAddress OpPush
        store z $ y + 1
        store (Register PC) $ x - 1

performOperation (SpecialInstruction INT b) =
    do
        return () -- Interrupt?  I'll make it later :P

performOperation (SpecialInstruction IAG b) =
    do
        z <- loadAddress b
        ia <- load $ Register IA
        store z ia

performOperation (SpecialInstruction IAS b) =
    do
        x <- loadValue b
        store (Register IA) x

performOperation (SpecialInstruction RFI b) =
    do
        x <- loadAddress OpPop
        y <- loadAddress OpPop
        store (Register IA) 0

performOperation (SpecialInstruction IAQ b) =
    do
        w <- loadValue b
        store (Register IA) w

performOperation (SpecialInstruction HWN b) =
    do
        z <- loadAddress b
        store z hwQuantity

performOperation (SpecialInstruction HWQ b) =
    do
        x <- loadValue b
        let (ab, c, xy) = hwInfo x
        store (Register A) $ fromIntegral $ ab .&. 0xFFFF
        store (Register B) $ fromIntegral $ shiftR ab 16
        store (Register C) c
        store (Register X) $ fromIntegral $ xy .&. 0xFFFF
        store (Register Y) $ fromIntegral $ shiftR xy 16

performOperation (SpecialInstruction HWI b) =
    do
        x <- loadValue b
        hwSendINT x

{- Placeholder and information
mainBody :: Emulator m => m ()
mainbody = do
            blah blah
            blah
            performOperation blah
            incrementPC             -- Must do this after performOperation
-}
