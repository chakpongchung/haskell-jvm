module Runtime.Thread.OperandStack (
    OperandStack(..),
    newOperandStack,
) where

import Data.Bits
import Data.Int
import Data.Text
import Data.Word
import qualified Data.List as L
import Common
import Control.Monad.State.Lazy
import Data.Binary.IEEE754

type MaxOperandStack = Int
type Index = Int

data OperandStack = OperandStack {
    otop :: Int,
    omaxNum :: Word16,
    oslot :: [VType]
} deriving (Show)

newOperandStack :: Word16 -> OperandStack
newOperandStack n = OperandStack {otop = 0,omaxNum = n,oslot = []}

