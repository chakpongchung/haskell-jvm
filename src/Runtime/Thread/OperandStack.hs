module Runtime.Thread.OperandStack (
    OperandStack(..),
    newOperandStack,
    pushInt,
    pushFloat,
    pushLong,
    pushDouble,
    pushReference,
    popInt,
    popFloat,
    popLong,
    popDouble,
    popReference

) where

import Data.Bits
import Data.Int
import Data.Text
import Data.Word
import qualified Data.List as L
import Common
import Control.Monad.State.Lazy

-- import Data.Array.MArray
-- import Control.Monad
-- import Control.Monad.ST
-- import Data.Array.ST
-- import Data.Array.Unboxed


type MaxOperandStack = Int
type Index = Int

-- type OperandStack = UArray Int Int32
-- newOperandStack :: MaxOperandStack -> UArray Int Int32
-- newOperandStack n = runSTUArray $ newArray_ (0,n)

data OperandStack = OperandStack {
    otop :: Int,
    maxNum :: Int,
    slot :: [DataType]
} deriving (Show)

newOperandStack :: Int -> OperandStack
newOperandStack n = OperandStack { otop = 0,maxNum = n,slot = []}

pushInt :: Int32 -> StateT OperandStack IO ()
pushInt v = do
    stack <- get
    guard (checkMaxNum (maxNum stack) 1 (L.length $ slot stack))
    modify(\o -> o {slot = TypeInt32 v : slot o, otop = otop o + 1})

pushFloat :: Float -> StateT OperandStack IO ()
pushFloat v = do
    stack <- get
    guard (checkMaxNum (maxNum stack) 1 (L.length $ slot stack))
    modify(\o -> o {slot = TypeFloat v : slot o, otop = otop o + 1})

pushLong :: Int64 -> StateT OperandStack IO ()
pushLong v = do
    stack <- get
    guard (checkMaxNum (maxNum stack) 2 (L.length $ slot stack))

    let low = fromIntegral (v :: Int64) :: Int32
    let high = fromIntegral (v `shiftR` 32) :: Int32
    modify(\o -> o {slot = TypeInt32 high : TypeInt32 low : slot o,otop = otop o + 2} )

pushDouble :: Double -> StateT OperandStack IO ()
pushDouble v = do
    stack <- get
    guard (checkMaxNum (maxNum stack) 2 (L.length $ slot stack))
    modify(\o -> o {slot = TypeDouble v : slot o, otop = otop o + 1})
    -- 临时占用2个位置
    modify(\o -> o {slot = TypeDouble v : slot o, otop = otop o + 1})

pushReference :: Object -> StateT OperandStack IO ()
pushReference v = do
    stack <- get
    guard (checkMaxNum (maxNum stack) 1 (L.length $ slot stack))
    modify(\o -> o {slot = TypeReference v : slot o, otop = otop o + 1})

popInt :: StateT OperandStack IO Int32
popInt = do
    os <- get
    let v = slot os
    guard (checkSlot v 1)
    let (TypeInt32 r:xs) = slot os
    modify(\o -> o {slot = xs, otop = otop o - 1})
    return r

popFloat :: StateT OperandStack IO Float
popFloat = do
    os <- get
    let v = slot os
    guard (checkSlot v 1)
    let (TypeFloat r:xs) = slot os
    modify(\o -> o {slot = xs, otop = otop o - 1})
    return r

popLong :: StateT OperandStack IO Int64
popLong = do
    os <- get
    let v = slot os
    guard (checkSlot v 2)
    let (TypeInt32 high:TypeInt32 low:xs) = slot os
    let wlow = fromIntegral (fromIntegral (low :: Int32) :: Word32) :: Int64
    let whigh = (fromIntegral (fromIntegral (high :: Int32) :: Word32) :: Int64) `shiftL` 32
    let value = (.|.) whigh wlow
    modify(\o -> o {slot = xs, otop = otop o - 2})
    return value

popDouble :: StateT OperandStack IO Double
popDouble = do
    os <- get
    let v = slot os
    guard (checkSlot v 2)
    let (TypeFloat r:xs) = slot os
    let (TypeDouble high:TypeDouble low:xs) = slot os
    modify(\o -> o {slot = xs, otop = otop o - 2})
    return low

popReference :: StateT OperandStack IO Object
popReference = do
    os <- get
    let v = slot os
    guard (checkSlot v 1)
    let (TypeReference r:xs) = slot os
    modify(\o -> o {slot = xs, otop = otop o - 1})
    return r

checkMaxNum :: Int -> Int -> Int -> Bool
checkMaxNum m n c
    | c + n > m = error "StackOverflow..."
    | m < 0 = error "stack error..."
    | otherwise = True

checkSlot :: [DataType] -> Index -> Bool
checkSlot dt i
    | L.null dt = error "Stack is null"
    | L.length dt < i = error "Stack IndexOutOfBoundsException"
    | otherwise = True