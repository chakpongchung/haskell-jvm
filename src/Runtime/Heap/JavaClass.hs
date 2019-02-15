module Runtime.Heap.JavaClass (

    JavaClass(..),
    newJavaClass,
) where

import Data.Word
import Data.Maybe
import ClassParser.ClassReader
import ClassParser.ClassMember
import ClassParser.ConstantPool
import ClassParser.ClassAttributes
import qualified Data.ByteString as L

data JavaClass = JavaClass {
    _accessFlags :: Word16,
    _thisClassName :: String,   -- ^ java/lang/Object
    _superClassName :: String,
    _interfaceNames :: [String],
    _fields :: [Field],
    _methods :: [Method],
    _classLoader :: ClassLoader,
    _superClass :: JavaClass,
    _interfaces :: [JavaClass],
    _instanceSlotCount :: Word,
    _staticSlotCount :: Word,
    _staticVariablePool :: Slot,
    _constantPool :: ConstantPool
} deriving (Show)

data Field = Field {
    _fAccessFlags :: Word16,
    _fName :: String,
    _fDesc :: String
} deriving (Show)

data Method = Method {
    _mAccessFlags :: Word16,
    _mName :: String,
    _mDesc :: String,
    _maxStack :: Word,
    _maxLocalTable :: Word,
    _code :: L.ByteString
}

newJavaClass :: ClassFile -> JavaClass
newJavaClass cf = 
    JavaClass {
        _accessFlags = accessFlags cf,
        _thisClassName = thisClassName cf,
        _superClassName = superClassName cf,
        _interfaceNames = interNames cf,
        _fields = newFields $ fields cf,
        _methods = newMethods $ methods cf,
        _classLoader = newClassLoader,
        -- _superClass :: JavaClass,
        -- _interfaces :: [JavaClass],
        -- _instanceSlotCount :: Word,
        -- _staticSlotCount :: Word,
        -- _staticVariablePool :: Slot,
        _constantPool = newConstantPool $ constantPool cf
    }

newFields :: [MemberInfo] -> [Field]
newFields [] = []
newFields (x:xs) = buildField x : newFields xs
    where 
        buildField m = Field {
            _fAccessFlags = mAccessFlags m,
            _fName = memName m,
            _fDesc = memDesc m
        }

newMethod :: [MemberInfo] -> [Method]
newMethod [] = []
newMethod (x:xs) = buildMethod x : newMethod xs
    where
        buildMethod :: MemberInfo -> Method
        buildMethod m = Method {
            _mAccessFlags = mAccessFlags m,
            _mName = memName m,
            _mDesc = memDesc m,
            _maxStack = getMaxNum maxStack,
            _maxLocalTable = getMaxNum maxLocals,
            _code = getMaxNum code
        }

        codeAttr :: Maybe AttributeInfo
        codeAttr = findCodeAttribute'

        getMaxNum :: CodeAttribute -> Word16 -> Word16
        getMaxNum f = case codeAttr of
                            Nothing -> -1
                            (Just ca) -> f ca