module Instruction where

import Memory
import Data.Word (Word16)
import Data.Bits

data BasicOpcode
    = SET
    | ADD
    | SUB
    | MUL
    | MLI
    | DIV
    | DVI
    | MOD
    | MDI
    | AND
    | BOR
    | XOR
    | SHR
    | ASR
    | SHL
    | IFB
    | IFC
    | IFE
    | IFN
    | IFG
    | IFA
    | IFL
    | IFU
    | ADX
    | SBX
    | STI
    | STD
    deriving (Show)

data SpecialOpcode
    = JSR
    | INT
    | IAG
    | IAS
    | RFI
    | IAQ
    | HWN
    | HWQ
    | HWI
    deriving (Show)

data Operand
    = OpRegister Register
    | OpRegisterPointer Register
    | OpRegisterNextWord Register
    | OpPush
    | OpPop
    | OpPeek
    | OpPick
    | OpSP
    | OpPC
    | OpEX
    | OpNextWordPointer
    | OpNextWord
    | OpLiteral Word16

instance Show Operand where
    show (OpRegister r)         = show r
    show (OpRegisterPointer r)  = "[" ++ show r ++ "]"
    show (OpRegisterNextWord r) = "[" ++ show r ++ " + next word]"
    show OpPush                 = "[--SP]"
    show OpPop                  = "[SP++]"
    show OpPeek                 = "[SP]"
    show OpPick                 = "[SP + next word]"
    show OpSP                   = "SP"
    show OpPC                   = "PC"
    show OpEX                   = "EX"
    show OpNextWordPointer      = "[next word]"
    show OpNextWord             = "next word"
    show (OpLiteral w)          = show w

data Instruction
    = BasicInstruction BasicOpcode Operand Operand
    | SpecialInstruction SpecialOpcode Operand
    deriving (Show)

--Instructions are of the form aaaaaabbbbbooooo
decodeInstruction :: Word16 -> Instruction
decodeInstruction inst = case o of
    0x0 -> SpecialInstruction (decodeSpecialOpcode b) (decodeOperandA a)
    _   -> BasicInstruction (decodeBasicOpcode o) (decodeOperandB b) (decodeOperandA a)
    where
        o  = 0x1F .&. inst --Bits 0-4
        b  = 0x1F .&. shiftR inst 5 --Bits 5-9
        a  = 0x3F .&. shiftR inst 10 --Bits 10-15

decodeBasicOpcode :: Word16 -> BasicOpcode
decodeBasicOpcode 0x01 = SET
decodeBasicOpcode 0x02 = ADD
decodeBasicOpcode 0x03 = SUB
decodeBasicOpcode 0x04 = MUL
decodeBasicOpcode 0x05 = MLI
decodeBasicOpcode 0x06 = DIV
decodeBasicOpcode 0x07 = DVI
decodeBasicOpcode 0x08 = MOD
decodeBasicOpcode 0x09 = MDI
decodeBasicOpcode 0x0A = AND
decodeBasicOpcode 0x0B = BOR
decodeBasicOpcode 0x0C = XOR
decodeBasicOpcode 0x0D = SHR
decodeBasicOpcode 0x0E = ASR
decodeBasicOpcode 0x0F = SHL
decodeBasicOpcode 0x10 = IFB
decodeBasicOpcode 0x11 = IFC
decodeBasicOpcode 0x12 = IFE
decodeBasicOpcode 0x13 = IFN
decodeBasicOpcode 0x14 = IFG
decodeBasicOpcode 0x15 = IFA
decodeBasicOpcode 0x16 = IFL
decodeBasicOpcode 0x17 = IFU
decodeBasicOpcode 0x1A = ADX
decodeBasicOpcode 0x1B = SBX
decodeBasicOpcode 0x1E = STI
decodeBasicOpcode 0x1F = STD


decodeSpecialOpcode :: Word16 -> SpecialOpcode
decodeSpecialOpcode 0x01 = JSR
decodeSpecialOpcode 0x08 = INT
decodeSpecialOpcode 0x09 = IAG
decodeSpecialOpcode 0x0A = IAS
decodeSpecialOpcode 0x0B = RFI
decodeSpecialOpcode 0x0C = IAQ
decodeSpecialOpcode 0x10 = HWN
decodeSpecialOpcode 0x11 = HWQ
decodeSpecialOpcode 0x12 = HWI

decodeOperandA :: Word16 -> Operand
decodeOperandA 0x18 = OpPop
decodeOperandA op 
    | op >= 0x20 && op <= 0x3f = OpLiteral op --TODO Figure out what the spec is talking about here
    | otherwise                  = decodeOperand op

decodeOperandB :: Word16 -> Operand
decodeOperandB 0x18 = OpPush
decodeOperandB op = decodeOperand op

decodeOperand :: Word16 -> Operand
decodeOperand op
    | op <= 0x07 = OpRegister $ register op
    | op <= 0x0f = OpRegisterPointer $ register $ op - 0x8
    | op <= 0x17 = OpRegisterNextWord $ register $ op - 0x10
    | otherwise    = case op of
        0x19 -> OpPeek
        0x1A -> OpPick
        0x1B -> OpSP
        0x1C -> OpPC
        0x1D -> OpEX
        0x1E -> OpNextWordPointer
        0x1F -> OpNextWord
    where
        register = toEnum . fromIntegral