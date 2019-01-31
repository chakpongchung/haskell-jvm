module Common(
    ClassName
    ,ClassContent
    ,PoolIndex
    ,readWord16s
) where

import qualified Data.ByteString.Lazy as L
import Data.Word
import Data.Binary.Get


type PoolIndex = Word16
type ClassName = String
type ClassContent = L.ByteString


readWord16s :: Int ->  Get [Word16]
readWord16s 0 = return []
readWord16s n = do
   x <- getWord16be
   y <- readWord16s (n-1)
   return $ x : y


