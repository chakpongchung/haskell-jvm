module Runtime.Thread.JvmStack (
    JvmStack(..),
    newJvmStack,
    StackSize
) where

import Runtime.Thread.JvmStackFrame
import Data.Word

type StackSize = Word

data JvmStack = JvmStack {
    maxSize :: Word,
    size :: Word,
    top :: JvmStackFrame
} deriving (Show)

newJvmStack :: StackSize -> JvmStack
newJvmStack s = JvmStack {maxSize = s,size = 0,top = RootStackFrame}
