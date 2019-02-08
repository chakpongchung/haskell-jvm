module Runtime.Thread.LocalVariableTable(
    LocalVariableTable(..),
    newLocalVariableTable,
    setValue,
    getInt,
    getFloat,
    getLong,
    getDouble,
    getReference
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


type Index = Int

data LocalVariableTable = LocalVariableTable {
    maxNum :: Int,
    slot :: [DataType]
} deriving (Show)

-- newTable :: Int -> UArray Int DataType
-- newTable n = runSTUArray $ newArray_ (0,n)

-- putValue array index value = writeArray

newLocalVariableTable :: Int -> LocalVariableTable
newLocalVariableTable n = LocalVariableTable { maxNum = n,slot = []}

setValue :: DataType -> StateT LocalVariableTable IO Int
setValue dt = do
    lvt <- get
    guard (checkMaxNum (maxNum lvt) 1 (L.length $ slot lvt))
    case dt of 
        TypeInt32 n -> modify(\lvt -> lvt{slot = slot lvt ++ TypeInt32 n : []})
        TypeFloat f -> modify(\lvt -> lvt{slot = slot lvt ++ TypeFloat f : []})
        TypeInt64 f -> do
            guard (checkMaxNum (maxNum lvt) 1 (L.length $ slot lvt))
            let low = fromIntegral (f :: Int64) :: Int32
            let high = fromIntegral (f `shiftR` 32) :: Int32
            lift $ print low
            lift $ print high
            modify(\lvt -> lvt{slot = slot lvt ++ TypeInt32 low : TypeInt32 high : []} )
        TypeDouble d -> do
            guard (checkMaxNum (maxNum lvt) 1 (L.length $ slot lvt))
            modify(\lvt -> lvt{slot = slot lvt ++ TypeDouble d : []} )
        TypeReference o -> modify(\lvt -> lvt{slot = slot lvt ++ TypeReference o : []} )
    lvt <- get
    return . L.length $ slot lvt
        
getInt :: Index -> StateT LocalVariableTable IO Int32
getInt i = do
    lvt <- get
    let xs = slot lvt
    guard (checkSlot xs i)
    let (TypeInt32 r) = xs !! i
    return r

getFloat :: Index -> StateT LocalVariableTable IO Float
getFloat i = do
    lvt <- get
    let xs = slot lvt
    guard (checkSlot xs i)
    let (TypeFloat r) = xs !! i
    return r

getLong :: Index -> StateT LocalVariableTable IO Int64
getLong i = do
    lvt <- get
    let xs = slot lvt
    guard (checkSlot xs 2)
    let (TypeInt32 low) = xs !! i
    let (TypeInt32 high) = xs !! (i + 1)
    let wlow = fromIntegral (fromIntegral (low :: Int32) :: Word32) :: Int64
    let whigh = (fromIntegral (fromIntegral (high :: Int32) :: Word32) :: Int64) `shiftL` 32
    let value = (.|.) whigh wlow
    return value

getDouble :: Index -> StateT LocalVariableTable IO Double
getDouble i = do
    lvt <- get
    let xs = slot lvt
    guard (checkSlot xs i)
    let (TypeDouble r) = xs !! i
    return r

getReference :: Index -> StateT LocalVariableTable IO Object
getReference i = do
    lvt <- get
    let xs = slot lvt
    guard (checkSlot xs i)
    let (TypeReference r) = xs !! i
    return r

checkMaxNum :: Int -> Int -> Int -> Bool
checkMaxNum m n c
    | c + n > m = error "StackOverflow..."
    | m < 0 = error "stack error..."
    | otherwise = True

checkSlot :: [DataType] -> Index -> Bool
checkSlot dt i
    | L.null dt = error "localvariabletable is null"
    | L.length dt < i = error "localvariabletable IndexOutOfBoundsException"
    | otherwise = True
    