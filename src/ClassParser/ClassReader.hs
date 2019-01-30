module ClassParser.ClassReader(

    ClassFile(..)
    ,parserClass

)where

import Common
import ClassParser.ConstantPool
import Data.List
import Data.Word
import qualified Data.ByteString as L
import qualified Control.Monad.Trans.State.Lazy as S
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe
import Data.Binary.Get


data ClassFile = ClassFile {
    magic :: Word32,
    minorVersion :: Word16,
    majorVersion :: Word16,
    constantPool :: ConstantPool,
    accessFlags :: Word16,
    thisClass :: Word16,
    superClass :: Word16,
    interfaces :: [Word16],
    fields     ::  [MemberInfo],
    methods    :: [MemberInfo],
    attributes :: [AttributeInfo]
}


data MemberInfo = MemberInfo

data AttributeInfo = AttributeInfo

type ByteNum = Int

jvmMagic :: Word32
jvmMagic = 0xCAFEBABE

checkJvmMagic :: Word32 -> Bool
checkJvmMagic m
    | m == jvmMagic    =   True
    | otherwise = error "java.lang.ClassFormatError: magic!"

checkJvmVersion :: Word16 -> Word16 -> Bool
checkJvmVersion minor major 
    | major == 45  = True
    | major `elem` [46..52] && 0 == minor = True
    | otherwise    = error "java.lang.UnsupportedClassVersionError!"
     


readWord16 :: Int ->  Get [Word16]
readWord16 0 = return []
readWord16 n = do
   x <- getWord16be
   y <- readWord16 (n-1)
   return (x:y)

-- readConstantPool :: Int -> Get [Word16]
-- readConstantPool 0 = return []
-- readConstantPool n = do
--     tag <- getWord8be



check :: Word16 -> Bool
check n 
    | n == 0 = error (show $ fromEnum n)
    | otherwise =  error (show $ fromEnum n)

parserClass :: Get [Word16]
parserClass = do

    magic <- getWord32be
    guard $ checkJvmMagic magic

    minor <- getWord16be
    major <- getWord16be
    guard $ checkJvmVersion minor major
    
    cn <- getWord16be
    p <- readWord16 $ fromEnum cn

    -- guard $ check cn

    -- accessFlags <- getWord16be
    -- thisClass <- getWord16be
    -- superClass <- getWord16be

    -- num <- getWord16be
    -- guard $ check num
    -- interfaces <- readWord16 n


    return p
    --    return $! ClassFile {
--         magic = magic,
--         minorVersion = "",
--         majorVersion = "",
--         constantPool = "",
--         accessFlags = "",
--         thisClass = "",
--         superClass = ""
--         interfaces = [Word16],
--         fields = [MemberInfo],
--         methods = [MemberInfo],
--         attributes = ""
   
--    }