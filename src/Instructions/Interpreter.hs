module Instructions.Interpreter (
    interpreter,

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


interpreter :: MemberInfo -> IO ()
interpreter m = do
    let codeAttribute = findCodeAttribute $ memberAttributes m
    let ml = maxLocals codeAttribute
    let ms = maxStack codeAttribute
    let byteCode = code codeAttribute
    -- TODO 新增命令行参数
    
    let threadFrame = newJvmStackFrame ml ms
    thread <- execStateT (pushJvmStackFrame threadFrame) $ newJvmThread 1024
    let c = B.unpack byteCode 

    frame <- evalStateT popJvmStackFrame thread
    runStateT (loop c frame) thread
    return ()
 
loop :: [Word8] -> JvmStackFrame -> StateT JvmThread IO ()
loop byteCode frame = do
    -- 
    let currentPc = nextPC frame
    modify(\o -> o {pc = currentPc})
    let reader = BytecodeReader {bpc = currentPc,bcode = byteCode}
    (opCode,reader') <- lift $ runStateT readOpCode reader
    -- 创建指令
    (ins,reader') <- lift $ runStateT (newInstruction opCode) reader'
    let nextpc = bpc reader'
    (r,newFrame) <- lift $ runStateT (setNextPc nextpc) frame
    -- let lvt = localtable newFrame
    -- lift $ print $ "byteCode: " ++ show byteCode
    lift $ print $ "opCode: " ++ show opCode ++ ",currentPc: " ++ show currentPc ++ ",nextpc: " ++ show nextpc ++ ",inst>>>>>>>>>>>>>>>" ++ show ins
    -- 执行指令
    newFrame' <- lift $ execStateT (executeInstruction ins currentPc) newFrame
    let opStack = operandStack newFrame'
    -- 修改操作数栈
    newFrame' <- lift $ execStateT (modifyStackFrameOperandStack opStack) newFrame'
    lift $ print $ "localVariableTable: " ++ show (localtable newFrame')
    lift $ print $ "operandStack: " ++ show opStack
    if currentPc <  length byteCode then loop byteCode newFrame'
                                 else return ()
    -- return ()

modifyStackFrameOperandStack :: OperandStack -> StateT JvmStackFrame IO ()
modifyStackFrameOperandStack os = modify(\o -> o {operandStack = os})