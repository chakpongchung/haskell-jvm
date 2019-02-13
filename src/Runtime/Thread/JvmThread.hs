module Runtime.Thread.JvmThread(
    newJvmThread,
    testThread,
    popJvmStackFrame,
    pushJvmStackFrame,
    JvmThread(..)
) where

import Control.Monad.State.Lazy
import Runtime.Thread.JvmStack
import Runtime.Thread.JvmStackFrame
import Runtime.Thread.OperandStack
import Runtime.Thread.LocalVariableTable

import Data.Array.ST
import Data.Int
import Common
import Data.Word


data JvmThread = JvmThread {
    pc :: Int,
    stack :: JvmStack
} deriving (Show)


newJvmThread :: StackSize -> JvmThread 
newJvmThread s = JvmThread { pc = 0,stack = newJvmStack s }


checkStackFull :: JvmStack -> Bool
checkStackFull s 
    | size s >= maxSize s = error "java.lang.StackOverflowError!"
    | otherwise = True

checkStackEmpty :: JvmStack -> Bool
checkStackEmpty s
    | size s == 0 = error "JVM Stack is Empty!"
    | otherwise = True

pushJvmStackFrame :: JvmStackFrame -> StateT JvmThread IO ()
pushJvmStackFrame sf = do
    thread <- get
    let st = stack thread
    guard $ checkStackFull st
    let newsf = sf{nextNode = top st}
    let newst = st{size = size st + 1, top = newsf}
    modify (\s -> s {stack = newst})

popJvmStackFrame :: StateT JvmThread IO JvmStackFrame
popJvmStackFrame = do
    thread <- get
    let st = stack thread
    guard $ checkStackEmpty st
    let ctop = top st
    let ntop = nextNode ctop 
    let newst = st{size = size st - 1, top = ntop}
    modify (\t -> t {stack = newst})
    return ctop

testThread :: StateT JvmThread IO ()
testThread = do
    pushJvmStackFrame $ newJvmStackFrame 3 4
    c1 <- get
    lift $ print c1
    pushJvmStackFrame $ newJvmStackFrame 3 4
    c2 <- get
    lift $ print c2
    pushJvmStackFrame $ newJvmStackFrame 3 4
    c3 <- get
    lift $ print c3
    popJvmStackFrame 
    popJvmStackFrame 
    popJvmStackFrame 
    lift $ print "#####check empty#######"
    -- popJvmStackFrame 
    return ()

