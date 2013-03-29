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

data Memory s = Memory (MV.MVector s Word16)

fromAddress :: Address -> Int
fromAddress (Word w) = fromIntegral w
fromAddress (Register r) = 0x10000 + fromEnum r

new :: ST s (Memory s)
new = do
    memory <- MV.replicate (0x10001 + fromEnum (maxBound :: Register)) 0x0
    return $ Memory memory

read :: Memory s -> Address -> ST s Word16
read (Memory a) = MV.read a . fromAddress

write :: Memory s -> Address -> Word16 -> ST s ()
write (Memory a) = MV.write a . fromAddress

swap :: Memory s -> Address -> Address -> ST s ()
swap (Memory a) b c = MV.swap a (fromAddress b) $ fromAddress c