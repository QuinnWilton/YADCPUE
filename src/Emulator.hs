{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}
module Emulator where

import Memory
import Instruction
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.Trans
import Data.Bits
import Data.Word (Word16, Word32)
import Debug.Trace

class Monad m => Emulator m where
	load  :: Address -> m Word16
	store :: Address -> Word16 -> m ()

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


-- Note:  All next word operands are incomplete, each have a placeholder return.
loadAddress :: Emulator m => Operand -> m Address
loadAddress (OpRegister r)	 		= return $ Register r
loadAddress (OpRegisterPointer p)	= liftM Word $ load $ Register p
loadAddress (OpRegisterNextWord w)	= return $ Word 1
loadAddress OpPush					= do { x <- load $ Register SP; store (Register SP) $ x-1; return . Word $ x-1 }
loadAddress OpPeek					= liftM Word $ load $ Register SP
loadAddress OpPick					= return $ Word 1
loadAddress OpSP					= return $ Register SP
loadAddress OpPC					= return $ Register PC
loadAddress OpEX					= return $ Register EX
loadAddress OpNextWordPointer		= return $ Word 1
loadAddress OpNextWord 				= return $ Word 1
loadAddress (OpLiteral _)			= return $ Word 1  -- Have not defined silent failure case for literal address writing

loadValue :: Emulator m => Operand -> m Word16
loadValue (OpRegister r)			= load $ Register r
loadValue (OpRegisterPointer p)		= load (Register p) >>= load . Word
loadValue (OpRegisterNextWord w)	= return 1
loadValue OpPop						= do { x <- load $ Register SP; y <- load $ Word x; store (Word x) 0; store (Register SP) $ x+1; return y }
loadValue OpPeek					= load (Register SP) >>= load . Word
loadValue OpPick					= return 1
loadValue OpSP						= load $ Register SP
loadValue OpPC 						= load $ Register PC
loadValue OpEX 						= load $ Register EX
loadValue OpNextWordPointer 		= return 1
loadValue OpNextWord 				= return 1
loadValue (OpLiteral w)				= return w

performOperation :: Emulator m => Instruction -> m ()
performOperation (BasicInstruction SET b c)
	= do 
		x <- loadAddress b
		y <- loadValue c
		store x y
performOperation (BasicInstruction ADD b c)
	= do
		x <- loadValue b
		y <- loadValue c
		z <- loadAddress b
		store (Register EX) $ if fromIntegral x + fromIntegral y > (0xFFFF :: Int) then 1 else 0
		store z $ x+y
performOperation (BasicInstruction SUB b c)
	= do
		x <- loadValue b
		y <- loadValue c
		z <- loadAddress b
		store (Register EX) $ if fromIntegral x - fromIntegral y < (0 :: Int) then 0xFFFF else 0
		store z $ x-y
performOperation (BasicInstruction MUL b c)
	= do
		x <- loadValue b
		y <- loadValue c
		z <- loadAddress b
		store (Register EX) $ fromIntegral (shiftR (fromIntegral x * fromIntegral y) 16 .&. 0xFFFF :: Word32)
		store z $ x*y
performOperation (BasicInstruction MLI b c)
	= do
		x <- loadValue b
		y <- loadValue c
		z <- loadAddress b
		return ()
performOperation (BasicInstruction DIV b c)
	= do
		x <- loadValue b
		y <- loadValue c
		z <- loadAddress b
		if y == 0 then do
			store z 0
			store (Register EX) 0
		else do
			store z (div x y)
			store (Register EX) $ fromIntegral (div (shiftL (fromIntegral x) 16) $ fromIntegral y .&. 0xFFFF :: Word32)
performOperation (BasicInstruction DVI b c)
	= do
		x <- loadValue b
		y <- loadValue c
		z <- loadValue b
		return ()
performOperation (BasicInstruction MOD b c) 
	= do
		x <- loadValue b
		y <- loadValue c
		z <- loadAddress b
		store z $ if y == 0 then 0 else mod x y
performOperation (BasicInstruction MDI b c)
	= do
		x <- loadValue b
		y <- loadValue c
		z <- loadAddress b
		return ()
performOperation (BasicInstruction AND b c)
	= do
		x <- loadValue b
		y <- loadValue c
		z <- loadAddress b
		store z (x .&. y)
performOperation (BasicInstruction BOR b c)
	= do
		x <- loadValue b
		y <- loadValue c
		z <- loadAddress b
		store z (x .|. y)
performOperation (BasicInstruction XOR b c)
	= do
		x <- loadValue b
		y <- loadValue c
		z <- loadAddress b
		store z (xor x y)
-- Todo:  ADD STUFFS



emumonad :: Emulator m => m ()
emumonad = do
	performOperation $ decodeInstruction 0x9001 -- SET    A,   4
	performOperation $ BasicInstruction SET OpPush (OpLiteral 0xDEAD) -- SET PUSH, 21
	performOperation $ BasicInstruction SET (OpRegister A) (OpLiteral 0xBEEF)  --
	performOperation $ BasicInstruction MUL (OpRegister A) OpPeek
	return ()


