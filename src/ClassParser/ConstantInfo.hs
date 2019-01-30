module ClassParser.ConstantInfo (
    ConstantInfo(..)
    ,newConstantInfo
    ,PoolIndex
) where

import Data.Binary.Get
import Data.Word
import Data.Int
import qualified Data.ByteString as L
import qualified Data.ByteString.Lazy.Char8 as C

type Tag = Word8
type PoolIndex = Word16

data ConstantInfo = ConstantIntegerInfo Int32
    | ConstantFloatInfo Float
    | ConstantLongInfo  Int64
    | ConstantDoubleInfo Double
    | ConstantUtf8Info String
    | ConstantStringInfo {stringIndex :: PoolIndex}
    | ConstantClassInfo {nameIndex :: PoolIndex}

    | ConstantFieldRefInfo {classIndex :: PoolIndex, nameAndTypeIndex :: PoolIndex}
    | ConstantMethodRefInfo {classIndex :: PoolIndex, nameAndTypeIndex :: PoolIndex}
    | ConstantInterfaceMethodRefInfo {classIndex :: PoolIndex, nameAndTypeIndex :: PoolIndex}

    | ConstantNameAndType {nameIndex :: PoolIndex, descIndex :: PoolIndex}
    | ConstantMethodType
    | ConstantMethodHandler
    | ConstantInvokeDynamic


constantClass :: Tag
constantClass   =   7

constantFieldRef :: Tag
constantFieldRef = 9

constantMethodRef :: Tag
constantMethodRef = 10

constantInterfaceMethodRef :: Tag
constantInterfaceMethodRef = 11

constantString :: Tag
constantString = 8

constantInteger :: Tag
constantInteger = 3

constantFloat :: Tag
constantFloat = 4

constantLong :: Tag
constantLong = 5

constantDouble :: Tag
constantDouble = 6

constantNameAndType :: Tag
constantNameAndType = 12

constantUtf8 :: Tag
constantUtf8 = 1

constantMethodHandler :: Tag
constantMethodHandler = 15

constantMethodType :: Tag
constantMethodType = 16

constantInvokeDynamic :: Tag
constantInvokeDynamic = 18


newConstantInfo :: Tag -> Get ConstantInfo
newConstantInfo tag
    | constantClass == tag  =  ConstantClassInfo <$> getWord16be
    | constantFieldRef == tag = ConstantFieldRefInfo <$> getWord16be <*> getWord16be
    | constantMethodRef == tag = ConstantMethodRefInfo <$> getWord16be <*> getWord16be
    | constantInterfaceMethodRef == tag  = ConstantInterfaceMethodRefInfo <$> getWord16be <*> getWord16be
    | constantString == tag  = ConstantStringInfo <$> getWord16be
    | constantInteger == tag  =  ConstantIntegerInfo <$> getInt32be
    | constantFloat == tag  = ConstantFloatInfo <$> getFloatbe
    | constantLong == tag  = ConstantLongInfo <$> getInt64be
    | constantDouble == tag  = ConstantDoubleInfo <$> getDoublebe
    | constantNameAndType == tag  = ConstantNameAndType <$> getWord16be <*> getWord16be
    | constantUtf8 == tag  = do
        num <- getWord16be
        bt <- getByteString $ fromEnum num
        return $ ConstantUtf8Info $ show bt
    -- | constantMethodHandler == tag  = ConstantMethodHandler
    -- | constantMethodType == tag  = ConstantMethodType
    -- | constantInvokeDynamic == tag  = ConstantInvokeDynamic
    | otherwise = error "java.lang.ClassFormatError: constant pool tag!"
