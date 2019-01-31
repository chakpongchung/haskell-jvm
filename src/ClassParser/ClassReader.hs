module ClassParser.ClassReader(

    ClassFile(..)
    ,parserClass

)where

import Common
import ClassParser.ConstantPool
import ClassParser.ClassMember
import ClassParser.ClassAttributes
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
    classAttributes :: [AttributeInfo]
}

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



check :: Word16 -> Bool
check n 
    | n == 0 = error ("nnnn" ++ (show $ fromEnum n))
    | otherwise =  error ("dddd" ++ (show $ fromEnum n))

parserClass :: Get [AttributeInfo]
parserClass = do

    -- read magic
    magic <- getWord32be
    guard $ checkJvmMagic magic

    -- read version
    minor <- getWord16be
    major <- getWord16be
    guard $ checkJvmVersion minor major

    -- read ConstantPool
    cp <- readConstantPool

    accessFlags <- getWord16be
    thisClass <- getWord16be
    superClass <- getWord16be

    -- read interfaces
    num <- getWord16be
    interfaces <- readWord16s $ fromEnum num

    -- read fields
    fields <- readMemberInfo cp
    -- read methods
    methods <- readMemberInfo cp
    -- -- read attributes
    attributes <- readAttributes cp
    return attributes
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
--         classAttributes = ""
   
--    }