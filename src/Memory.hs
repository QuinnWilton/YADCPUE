module Memory where

import Data.Word (Word16)

data Register = A | B | C | X | Y | Z | I | J
    deriving (Enum, Show, Eq)

data Address
    = Register Register
    | SP
    | PC
    | EX
    | IA
    | Word Word16
    deriving (Eq)

instance Show Address where
    show (Register r) = show r
    show SP = "SP"
    show PC = "PC"
    show EX = "EX"
    show IA = "IA"
    show (Word w) = "[" ++ show w ++ "]"