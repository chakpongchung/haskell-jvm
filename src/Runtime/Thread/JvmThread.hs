module Runtime.Thread.JvmThread(
    newJvmThread,
    newStackFrame,
    testThread,
    StackFrame(..)
) where

import Control.Monad.State.Lazy
import Runtime.Thread.LocalVariableTable
import Runtime.Thread.OperandStack
import Data.Array.ST
import Data.Int

type StackSize = Word

data JvmThread = JvmThread {
    pc :: Int,
    stack :: JvmStack
} deriving (Show)

data JvmStack = JvmStack {
    maxSize :: Word,
    size :: Word,
    top :: StackFrame
} deriving (Show)

data StackFrame = RootStackFrame | StackFrame {
    nextNode :: StackFrame,
    localVariableTable :: LocalVariableTable,
    operandStack :: OperandStack
} deriving (Show)

newJvmThread :: StackSize -> JvmThread 
newJvmThread s = JvmThread { pc = 0,stack = newJvmStack s }

newJvmStack :: StackSize -> JvmStack
newJvmStack s = JvmStack {maxSize = s,size = 0,top = RootStackFrame}

newStackFrame :: Int -> Int -> StackFrame
newStackFrame t s = StackFrame {
    nextNode = RootStackFrame,
    localVariableTable = newLocalVariableTable t,
    operandStack = newOperandStack s
}

checkStackFull :: JvmStack -> Bool
checkStackFull s 
    | size s >= maxSize s = error "java.lang.StackOverflowError!"
    | otherwise = True

checkStackEmpty :: JvmStack -> Bool
checkStackEmpty s
    | size s == 0 = error "JVM Stack is Empty!"
    | otherwise = True

pushStackFrame :: StackFrame -> StateT JvmThread IO ()
pushStackFrame sf = do
    thread <- get
    let st = stack thread
    guard $ checkStackFull st
    let newsf = sf{nextNode = top st}
    let newst = st{size = size st + 1, top = newsf}
    modify (\s -> s {stack = newst})

popStackFrame :: StateT JvmThread IO ()
popStackFrame = do
    thread <- get
    let st = stack thread
    guard $ checkStackEmpty st
    let ctop = top st
    let ntop = nextNode ctop 
    let newst = st{size = size st - 1, top = ntop}
    modify (\t -> t {stack = newst})

testThread :: StateT JvmThread IO ()
testThread = do
    pushStackFrame $ newStackFrame 3 4
    c1 <- get
    lift $ print c1
    pushStackFrame $ newStackFrame 3 4
    c2 <- get
    lift $ print c2
    pushStackFrame $ newStackFrame 3 4
    c3 <- get
    lift $ print c3
    popStackFrame 
    popStackFrame 
    popStackFrame 
    lift $ print "############"
    popStackFrame 
    return ()





