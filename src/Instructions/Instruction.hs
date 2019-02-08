module Instructions.Instruction (

) where

import Data.Word

data Instruction = 
     NoOperandInstruction 
    | BranchInstruction {offset :: Int}
    | StoreAndLoadInstruction {index :: Word8}
    | Index16Instruction {index :: Word8}