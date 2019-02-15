module Runtime.Thread.JvmStackFrame (
    JvmStackFrame(..),
    newJvmStackFrame,
    pushValue,
    pushInt,
    pushFloat,
    pushLong,
    pushDouble,
    pushReference,
    popValue,
    popInt,
    popFloat,
    popLong,
    popDouble,
    popReference,

    setInt,
    setFloat,
    setLong,
    setDouble,
    setReference,
    getInt,
    getFloat,
    getLong,
    getDouble,
    getReference,

    setNextPc
) where

import Runtime.Thread.LocalVariableTable
import Runtime.Thread.OperandStack
import Runtime.Heap.Object
import Common
import Data.Word
import Data.Int
import Data.Bits
import Data.Maybe
import qualified Data.List as L
import Data.Binary.IEEE754
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy

data JvmStackFrame = RootStackFrame | JvmStackFrame {
    nextNode :: JvmStackFrame,
    localtable :: LocalVariableTable,
    operandStack :: OperandStack,
    nextPC :: Int
} deriving (Show)

newJvmStackFrame :: Word16 -> Word16 -> JvmStackFrame
newJvmStackFrame t s = JvmStackFrame {
    nextNode = RootStackFrame,
    localtable = newLocalVariableTable t,
    operandStack = newOperandStack s,
    nextPC = 0
}

setNextPc :: Int -> StateT JvmStackFrame IO ()
setNextPc pc = modify(\o -> o{nextPC = pc})

setValue :: Index -> VType -> StateT JvmStackFrame IO ()
setValue i t = do
    jsf <- get
    let lvt = localtable jsf
    -- guard (checkMaxNum (maxNum lvt) 1 (Map.size $ slot lvt))
    -- lift $ print $ "set-int=============: " ++ show i ++ ":" ++ show t
    let lvt' = LocalVariableTable {maxNum = maxNum lvt,slot = Map.insert i t $ slot lvt}
    modify(\o -> o{localtable = lvt'})

setInt :: Index -> Int32 -> StateT JvmStackFrame IO ()
setInt index value = setValue index (VInt value)

setFloat :: Index -> Float -> StateT JvmStackFrame IO ()
setFloat index value = do
    let w32 = floatToWord value
    let v = fromIntegral w32 :: Int32
    setValue index (VInt v)

setLong :: Index -> Int64 -> StateT JvmStackFrame IO ()
setLong index value = do
    let low = fromIntegral (value :: Int64) :: Int32
    let high = fromIntegral (value `shiftR` 32) :: Int32
    setValue index (VInt low)
    setValue (index + 1) (VInt high)

setDouble :: Index -> Double -> StateT JvmStackFrame IO ()
setDouble index value = do
    let w64 = doubleToWord value
    let v = fromIntegral w64 :: Int64
    setLong index v

setReference :: Index -> Object -> StateT JvmStackFrame IO ()
setReference index value = setValue index (VRef value)
    
getValue :: Index -> StateT JvmStackFrame IO VType
getValue i = do
    jsf <- get
    let table = localtable jsf
    let xs = slot table
    let v = fromMaybe (error $ "not found value from LocalVariableTable" ++ show i) $ Map.lookup i xs
    return v

getInt :: Index -> StateT JvmStackFrame IO Int32
getInt i = do
    (VInt v) <- getValue i
    return v

getFloat :: Index -> StateT JvmStackFrame IO Float
getFloat i = do
    v <- getInt i
    let f = wordToFloat $ (fromIntegral v :: Word32)
    return f

getLong :: Index -> StateT JvmStackFrame IO Int64
getLong i = do
    low <- getInt i
    high <- getInt (i + 1)
    let wlow = fromIntegral (fromIntegral (low :: Int32) :: Word32) :: Int64
    let whigh = (fromIntegral (fromIntegral (high :: Int32) :: Word32) :: Int64) `shiftL` 32
    let value = (.|.) whigh wlow
    return value

getDouble :: Index -> StateT JvmStackFrame IO Double
getDouble i = do
    v <- getLong i
    let d = wordToDouble $ (fromIntegral v :: Word64)
    return d

getReference :: Index -> StateT JvmStackFrame IO Object
getReference i = do
    (VRef o) <- getValue i
    return o

pushValue :: VType -> StateT JvmStackFrame IO ()
pushValue v = do
    jsf <- get
    -- lift $ print $ "add-current-jsf1: " ++ show jsf
    let stack = operandStack jsf
    guard (checkMaxNum (omaxNum stack) 1 (L.length $ oslot stack))
    let n = otop stack
    -- let stack' = OperandStack {omaxNum = omaxNum stack,oslot = oslot stack ++ v : [],otop = n + 1}
    let stack' = OperandStack {omaxNum = omaxNum stack,oslot = v : oslot stack,otop = n + 1}
    modify(\o -> o {operandStack = stack'})
    -- jsf' <- get
    -- lift $ print $ "add-current-jsf2: " ++ show jsf'

pushInt :: Int32 -> StateT JvmStackFrame IO ()
pushInt = pushValue . VInt

pushFloat :: Float -> StateT JvmStackFrame IO ()
pushFloat f = do
    let v = (fromIntegral $ floatToWord f) :: Int32
    pushInt v

pushLong :: Int64 -> StateT JvmStackFrame IO ()
pushLong v = do
    let low = fromIntegral (v :: Int64) :: Int32
    let high = fromIntegral (v `shiftR` 32) :: Int32
    pushInt low
    pushInt high

pushDouble :: Double -> StateT JvmStackFrame IO ()
pushDouble v = do
    let value = (fromIntegral $ doubleToWord v) :: Int64
    pushLong value

pushReference :: Object -> StateT JvmStackFrame IO ()
pushReference v = pushValue $ VRef v
    
    
popValue :: StateT JvmStackFrame IO VType
popValue = do
    jsf <- get
    -- lift $ print $ "current-jsf: " ++ show jsf
    let stack = operandStack jsf
    -- guard (checkSlot (oslot stack) (i - 1))
    let (x,xs) = case oslot stack of
            (a:[]) -> (a,[])
            (a:xa) -> (a,xa)

    -- let (x:xs) = oslot stack
    -- lift $ print $ "pop^value: " ++ show x ++ ",stack: " ++ (show $ oslot stack) 
    let stack' = OperandStack {omaxNum = omaxNum stack,oslot = xs,otop = otop stack - 1}
    -- let stack' = OperandStack {omaxNum = omaxNum stack,otop = i - 1,oslot = oslot stack}
    modify(\o -> o {operandStack = stack'})
   
    return x

popInt :: StateT JvmStackFrame IO Int32
popInt = do
    (VInt r) <- popValue
    return r

popFloat :: StateT JvmStackFrame IO Float
popFloat = do
    (VInt r) <- popValue
    let f = wordToFloat $ (fromIntegral r :: Word32)
    return f

popLong :: StateT JvmStackFrame IO Int64
popLong = do
    (VInt high) <- popValue
    (VInt low) <- popValue
    let wlow = fromIntegral (fromIntegral (low :: Int32) :: Word32) :: Int64
    let whigh = (fromIntegral (fromIntegral (high :: Int32) :: Word32) :: Int64) `shiftL` 32
    let value = (.|.) whigh wlow
    return value

popDouble :: StateT JvmStackFrame IO Double
popDouble = do
    value <- popLong
    let d = wordToDouble $ (fromIntegral value :: Word64)
    return d

popReference :: StateT JvmStackFrame IO Object
popReference = do
    (VRef o) <- popValue
    return o

checkMaxNum :: Word16 -> Int -> Int -> Bool
checkMaxNum m n c
    -- | c + n > max = error "StackOverflow..."
    | max < 0 = error "stack error..."
    | otherwise = True
    where max = fromIntegral m :: Int


checkSlot :: [VType] -> Index -> Bool
checkSlot dt i
    | L.null dt = error $ "Stack is null: " ++ show i
    | L.length dt < i = error "Stack IndexOutOfBoundsException"
    | i < 0 = error "Stack Index Error"
    | otherwise = True