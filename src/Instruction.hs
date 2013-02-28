module Instruction where

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

data Instruction a
    = BasicInstruction BasicOpcode a a
    | SpecialInstruction SpecialOpcode a
    deriving (Show)

decodeInstruction :: Word16 -> Instruction Word16
decodeInstruction inst = case o of
    0x0 -> SpecialInstruction (decodeSpecialOpcode b) a
    _   -> BasicInstruction (decodeBasicOpcode o) b a
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