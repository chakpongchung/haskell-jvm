module ClassParser.ConstantPool (
    ConstantInfo(..)
    ,readConstantPool
    ,ConstantPool
    ,Tag
    ,getUtf8
    ,getInterfaceName
    ,getNameAndDesc
) where

import Data.Binary.Get
import Data.Word
import Common
import Data.Int
import qualified Data.ByteString as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.String.Conversions
import Control.Monad

type ConstantPool = [ConstantInfo]
type Tag = Word8

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
    | ConstantMethodType {descIndex :: PoolIndex}
    | ConstantMethodHandler {referenceKind :: Tag, referenceIndex :: PoolIndex}
    | ConstantInvokeDynamic {bootstrapMethodAttrIndex :: PoolIndex, nameAndTypeIndex :: PoolIndex}
    deriving (Show)


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

newConstantInfo :: Int -> Tag -> Get ConstantInfo
newConstantInfo n tag
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
        -- guard $ test bt
        return $ ConstantUtf8Info $ "" ++ (cs bt :: String) ++ ""
    | constantMethodHandler == tag  = ConstantMethodHandler <$> getWord8 <*> getWord16be
    | constantMethodType == tag  = ConstantMethodType <$> getWord16be
    | constantInvokeDynamic == tag  = ConstantInvokeDynamic <$> getWord16be <*> getWord16be
    | otherwise = error $ "java.lang.ClassFormatError: constant pool tag：" ++ show tag ++ "-----" ++ show n

readConstantPool :: Get ConstantPool
readConstantPool = do
    cn <- getWord16be
    newConstantPool (fromEnum cn)

newConstantPool :: Int -> Get ConstantPool
newConstantPool 1 = return []
newConstantPool n = do
    tag <- getWord8
    x <- newConstantInfo n tag
    let size = skipBytes tag
    y <- newConstantPool $ n - size
    let r = if size == 1 then  x : y else x : padding tag : y
    return r

skipBytes :: Tag -> Int
skipBytes t
    | t == constantLong = 2
    | t == constantDouble = 2
    | otherwise = 1

-- Long/Double need 2 bytes，use for 1 bytes
-- TODO need better implementation
padding :: Tag -> ConstantInfo
padding t
    | t == constantLong = ConstantLongInfo 0
    | otherwise = ConstantDoubleInfo 0

getUtf8 :: PoolIndex -> ConstantPool -> String
getUtf8 i c = 
    case constantInfo i c of
        ConstantUtf8Info str -> str
        -- read (this/super)class name
        ConstantClassInfo nameIndex -> getUtf8 nameIndex c
        _  -> error "type error, index: " ++ show i

-- 获取名称和描述符        
getNameAndDesc :: PoolIndex -> ConstantPool -> (String,String)
getNameAndDesc i c = 
    case constantInfo i c of
        ConstantNameAndType nameIndex descIndex -> (getUtf8 nameIndex c,getUtf8 descIndex c)
        _  -> error "type error, index: " ++ show i                             


getConstantInfo :: PoolIndex -> ConstantPool -> ConstantInfo
getConstantInfo i c = 
    if index >= clen 
        then error ("invalid constant index : " ++ show index ++ ", pool size: " ++ show clen)
        else c !! (index - 1)
    where 
        clen = length c
        index = fromEnum i

getInterfaceName :: ConstantPool -> [Word16] -> [String]
getInterfaceName c [] = []
getInterfaceName c xs = map (`getUtf8` c) xs

test :: L.ByteString -> Bool
test c 
    | True = error (cs c :: String)
    | otherwise = error (cs c :: String)