module Instructions.BytecodeReader(

) where

import qualified Data.ByteString as L
import Control.Monad.State.Lazy
import Data.Word
import Data.Bits
import Data.Int

data BytecodeReader = BytecodeReader {
    pc :: Int,
    code :: [Word8]
}

readUint8 :: StateT BytecodeReader IO Word8
readUint8 = do
   br <- get
   let codes = code br
   let i = pc br
   modify (\o -> o {pc = i + 1})
   return $ codes !! i

readInt8 :: StateT BytecodeReader IO Int8
readInt8 = do
    v <- readUint8
    return (fromIntegral v :: Int8)

readUint16 :: StateT BytecodeReader IO Word16
readUint16 = do
   high <- readUint8
   low <- readUint8
   let whigh = (fromIntegral high :: Word16) `shiftL` 8
   let wlow = fromIntegral high :: Word16
   let value = (.|.) whigh wlow
   return value

readInt16 :: StateT BytecodeReader IO Int16
readInt16 = do
   w <- readUint16
   return (fromIntegral w :: Int16)

readInt32 :: StateT BytecodeReader IO Int32
readInt32 = do
   b1 <- readUint8
   b2 <- readUint8
   b3 <- readUint8
   b4 <- readUint8
   let b1' = (fromIntegral b1 :: Int32) `shiftL` 24
   let b2' = (fromIntegral b2 :: Int32) `shiftL` 16
   let b3' = (fromIntegral b3 :: Int32) `shiftL` 8
   let b4' = fromIntegral b4 :: Int32
   return $! b1' .|. b2' .|. b3' .|. b4'

readInt32s :: StateT BytecodeReader IO ()
readInt32s = return ()

skipPadding :: StateT BytecodeReader IO ()
skipPadding = return ()