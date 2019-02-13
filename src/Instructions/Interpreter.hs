module Instructions.Interpreter (

) where

import ClassParser.ClassAttributes
import ClassParser.ClassReader
import ClassParser.ClassMember
import Runtime.Thread.JvmThread
import Runtime.Thread.OperandStack
import Runtime.Thread.JvmStackFrame
import Instructions.BytecodeReader
import Instructions.Instruction
import Control.Monad.State.Lazy
import qualified Data.ByteString as B
import Data.Word

data Interpreter = Interpreter


interpreter :: MemberInfo -> IO ()
interpreter m = do
    let codeAttribute = findCodeAttribute $ memberAttributes m
    let ml = maxLocals codeAttribute
    let ms = maxStack codeAttribute
    let byteCode = code codeAttribute
    -- TODO 1024需要增加命令行参数实现动态化
    let thread = newJvmThread 1024
    let threadFrame = newJvmStackFrame ml ms
    runStateT (pushJvmStackFrame threadFrame) thread
    let c = B.unpack byteCode

    frame <- execStateT popJvmStackFrame thread
    -- loop byteCode thread
    return ()
 
loop :: [Word8] -> JvmStackFrame -> StateT JvmThread IO ()
loop byteCode frame = do
    -- 
    let npc = nextPC frame
    modify(\o -> o {pc = npc})
    let reader = BytecodeReader {bpc = npc,bcode = byteCode}
    opCode <- lift $ readOpCode reader
    --
    ins <- lift $ evalStateT (newInstruction opCode) reader
    let nextpc = bpc reader
    newFrame <- lift $ modifyStackFrameNextPC nextpc frame
    let opStack = operandStack newFrame
    let lvt = localtable newFrame
    n <- lift $ execStateT (executeInstruction ins) newFrame
    newFrame' <- lift $ modifyStackFrameOperandStack opStack newFrame
    return ()


modifyStackFrameNextPC :: Int -> JvmStackFrame -> IO JvmStackFrame
modifyStackFrameNextPC p = execStateT $ modify(\o -> o {nextPC = p})

modifyStackFrameOperandStack :: OperandStack -> JvmStackFrame -> IO JvmStackFrame
modifyStackFrameOperandStack os = execStateT $ modify(\o -> o {operandStack = os})