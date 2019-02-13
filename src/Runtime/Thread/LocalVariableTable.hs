{-# LANGUAGE FlexibleContexts #-}
module Runtime.Thread.LocalVariableTable(
    LocalVariableTable(..),
    newLocalVariableTable,
) where

import Data.Bits
import Data.Int
import Data.Text
import Data.Maybe
import Data.Word
import qualified Data.List as L
import Common
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map

data LocalVariableTable = LocalVariableTable {
    maxNum :: Word16,
    slot :: Map.Map Int VType
} deriving (Show)

newLocalVariableTable :: Word16 ->  LocalVariableTable
newLocalVariableTable n = LocalVariableTable {maxNum = n,slot = Map.empty}


