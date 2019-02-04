{-# LANGUAGE DuplicateRecordFields #-}

module ClassParser.ClassAttributes(
    readAttributes
    ,AttributeInfo(..)
) where

import Common
import ClassParser.ConstantPool
import Data.Word
import Data.Binary.Get
import qualified Data.ByteString as L
import Control.Monad

type AttributeCount = Int
type AttributeName = String
type AttributeLens = Word32

data AttributeInfo =  
    CodeAttribute {maxStack :: Word16, maxLocals :: Word16, code :: L.ByteString, exceptionTable :: [ExceptionTableEntry], attributes :: [AttributeInfo]}
    | ConstantValueAttribute {constantValueIndex :: PoolIndex}
    | DeprecatedAttribute
    | SyntheticAttribute
    | ExceptionsAttribute {exceptionIndexTable :: [Word16]}
    | LineNumberTableAttribute {lineNumberTable :: [LineNumberTableEntry]}
    | LocalVariableTableAttribute {localVariableTable :: [LocalVariableTableEntry]}
    | SourceFileAttribute {sorceFileIndex :: Word16}
    | UnParserAttributeInfo {aName :: String,aLen :: Word32, info :: L.ByteString}
    deriving (Show)

data ExceptionTableEntry = ExceptionTableEntry {
    startPc :: Word16,
    endPc :: Word16,
    handlerPc :: Word16,
    catchType :: Word16
} deriving (Show)


data LineNumberTableEntry = LineNumberTableEntry {
    startPc :: Word16,
    lineNumber :: Word16
} deriving (Show)

data LocalVariableTableEntry = LocalVariableTableEntry {
    startPc :: Word16,
    length :: Word16,
    nameIndex :: Word16,
    descIndex :: Word16,
    index :: Word16
} deriving (Show) 

readAttributes :: ConstantPool -> Get [AttributeInfo]
readAttributes pool = do
    attrCount <- getWord16be
    newAttributeInfo' pool $ fromEnum attrCount

newAttributeInfo' :: ConstantPool -> AttributeCount -> Get [AttributeInfo]
newAttributeInfo' _ 0 = return []
newAttributeInfo' pool n = do
    attrNameIndex <- getWord16be
    -- 获取属性名称
    let attrName = getUtf8 attrNameIndex pool 
    attrLens <- getWord32be
    -- guard $ check attrNameIndex n attrName attrLens
    x <- newAttributeInfo pool attrName attrLens
    y <- newAttributeInfo' pool $ n - 1
    return $ x : y

readExceptionTableEntry :: Int -> Get [ExceptionTableEntry]
readExceptionTableEntry 0 = return []
readExceptionTableEntry n = do
    startPc <- getWord16be
    endPc <- getWord16be
    handlerPc <- getWord16be
    catchType <- getWord16be
    next <- readExceptionTableEntry $ n - 1
    return $ ExceptionTableEntry{startPc = startPc,endPc = endPc,handlerPc = handlerPc,catchType = catchType} : next

readLineNumberTableEntry :: Int -> Get [LineNumberTableEntry]
readLineNumberTableEntry 0 = return []
readLineNumberTableEntry n = do
    startPc <- getWord16be
    lineNumber <- getWord16be
    next <- readLineNumberTableEntry $ n - 1
    return $ LineNumberTableEntry { startPc = startPc,lineNumber = lineNumber } : next

readLocalVariableTableEntry :: Int -> Get [LocalVariableTableEntry]
readLocalVariableTableEntry 0 = return []
readLocalVariableTableEntry n = do
    startPc <- getWord16be
    length <- getWord16be
    nameIndex <- getWord16be
    descIndex <- getWord16be
    index <- getWord16be
    next <- readLocalVariableTableEntry $ n - 1
    return $ LocalVariableTableEntry { startPc = startPc,length = length,nameIndex = nameIndex, descIndex = descIndex, index = index} : next

newAttributeInfo :: ConstantPool -> AttributeName -> AttributeLens -> Get AttributeInfo
newAttributeInfo pool name len
    | name == "Code"  =  do
        maxStack <- getWord16be
        maxLocals <- getWord16be
        -- read code
        codeLength <- getWord32be
        code <- getByteString $ fromEnum codeLength
        -- read exception table
        exceptionTableLength <- getWord16be
        exceptionTable <- readExceptionTableEntry $ fromEnum exceptionTableLength
        -- read attributes
        attrCount <- getWord16be
        attributes <- newAttributeInfo' pool $ fromEnum attrCount
        return $ CodeAttribute {maxStack = maxStack,maxLocals = maxLocals,code = code,exceptionTable = exceptionTable,attributes = attributes}
    | name == "ConstantValue" =  ConstantValueAttribute <$> getWord16be
    | name == "Deprecated"  = return DeprecatedAttribute
    | name == "Synthetic"   =  return SyntheticAttribute
    | name == "Exceptions"  = do
            exceptionNum <- getWord16be
            rs <- readWord16s $ fromEnum exceptionNum
            return $ ExceptionsAttribute rs
    | name == "LineNumberTable" = do
        lineNumberTableNum <- getWord16be
        entry <- readLineNumberTableEntry $ fromEnum lineNumberTableNum
        return $ LineNumberTableAttribute entry 
    | name == "LocalVariableTable"  = do
        localVariableTableNum <- getWord16be
        entry <- readLocalVariableTableEntry $ fromEnum localVariableTableNum 
        return $ LocalVariableTableAttribute entry
    | name == "SourceFile"  =  SourceFileAttribute <$> getWord16be
    
    | otherwise = do
        -- guard (unKnow name len)
        info <- getByteString $ fromEnum len
        return $ UnParserAttributeInfo {aName = name,aLen = len,info = info}

unKnow :: AttributeName -> AttributeLens -> Bool
unKnow n c 
    | n == "d" = True
    | otherwise = error $ "~~~~~~~~" ++ n ++ "~~~~~~~~" ++ (show $ fromEnum c)

check :: Word16 -> Int -> String -> Word32-> Bool
check n c m s
    | m == "\"ConstantValue\"" = True
    | otherwise =  error $ "second: " ++ (show $ fromEnum n) ++ "----" ++ show  c ++ ": " ++ m ++ "=======" ++ (show $ fromEnum s)
    