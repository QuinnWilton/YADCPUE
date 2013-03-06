module Memory where

import Data.Word (Word16)
import Data.Vector.Mutable as MV
import Control.Monad.ST

data Register = A | B | C | X | Y | Z | I | J | SP | PC | EX | IA
    deriving (Bounded, Enum, Eq, Ord, Show)

data Address
    = Register Register
    | Word Word16
    deriving (Eq)

instance Show Address where
    show (Register r) = show r
    show (Word w) = "[" ++ show w ++ "]"

type Memory s = (ST s (MV.MVector s Word16))

fromAddress :: Address -> Int
fromAddress (Word w) = fromIntegral w
fromAddress (Register r) = 0x10001 + fromEnum r

new :: Memory s
new = do
    memory <- MV.replicate (0x10001 + fromEnum (maxBound :: Register)) 0x0
    write memory (fromEnum SP) 0xFFFF
    return memory

load :: Memory s -> Address -> ST s Word16
load a b = a >>= flip MV.read (fromAddress b)

store :: Memory s -> Address -> Word16 -> ST s ()
store a b c = a >>= \x -> MV.write x (fromAddress b) c