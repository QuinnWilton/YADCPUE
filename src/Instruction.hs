module Instruction where

import Data.Word (Word16)

data BasicInstruction
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
	| IFG
	| IFA
	| IFL
	| IFU
	| ADX
	| SBX
	| STI
	| STD
	deriving (Show)

data SpecialInstruction
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
	= BasicInstruction BasicInstruction a a
	| SpecialInstruction SpecialInstruction a