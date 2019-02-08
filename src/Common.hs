module Common(
    ClassName
    ,ClassContent
    ,PoolIndex
    ,readWord16s
    ,Slot
    ,Object(..)
    ,DataType(..)
) where

import qualified Data.ByteString.Lazy as L
import Data.Word
import Data.Binary.Get
import Data.Int

type PoolIndex = Word16
type ClassName = String
type ClassContent = L.ByteString

readWord16s :: Int ->  Get [Word16]
readWord16s 0 = return []
readWord16s n = do
    x <- getWord16be
    y <- readWord16s (n-1)
    return $ x : y

data Slot = IntT Int32 | FloatT Float | LongT Int64 | DoubleT Double| ReferenceT String deriving(Show)




data Object = Object {
    name :: String
} deriving (Show)

data DataType = TypeInt32 Int32 | TypeFloat Float | TypeInt64 Int64 | TypeDouble Double | TypeReference Object deriving (Show)



