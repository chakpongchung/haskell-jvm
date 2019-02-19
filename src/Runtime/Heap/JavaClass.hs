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

-- java类信息/字段信息/方法信息等将会保存到方法区
data JavaClass = JavaClass {
    _accessFlag :: Word16,          -- ^ 类访问标识
    _thisClassName :: String,       -- ^ 当前类名称，全限定名，如：java/lang/Object
    _superClassName :: String,      -- ^ 超类名，全限定名
    _interfaceNames :: [String],    -- ^ 接口名称列表，全限定名
    _fields :: [Field],             -- ^ 字段表
    _methods :: [Method],           -- ^ 方法表
    _classLoader :: ClassLoader,    -- ^ 类加载器
    _superClass :: JavaClass,       -- ^ 超类集合
    _interfaces :: [JavaClass],     -- ^ 接口类集合
    _instanceSlotCount :: Word,     -- ^ 实例变量占据的空间大小    
    _staticSlotCount :: Word,       -- ^ 静态变量占据的空间大小
    _staticVariables :: Slot,       -- ^ 静态变量
    _constantPool :: ConstantPool   -- ^ 运行时常量池
} deriving (Show)

data Field = Field {
    _fAccessFlag :: Word16,     -- ^ 字段访问标识
    _fName :: String,           -- ^ 字段名称
    _fDesc :: String            -- ^ 字段描述符
} deriving (Show)

data Method = Method {
    _mAccessFlag :: Word16,     -- ^ 方法访问标识
    _mName :: String,           -- ^ 方法名称
    _mDesc :: String,           -- ^ 方法描述符
    _maxStack :: Word,          -- ^ 操作数栈大小(由编译器计算)
    _maxLocalTable :: Word,     -- ^ 局部变量表大小(由编译器计算)
    _code :: L.ByteString       -- ^ 方法字节码
}

newJavaClass :: ClassFile -> JavaClass
newJavaClass cf = 
    JavaClass {
        _accessFlag = accessFlags cf,
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

-- 将MemberInfo(来自于ClassFile)转换成Field
newFields :: [MemberInfo] -> [Field]
newFields [] = []
newFields (x:xs) = buildField x : newFields xs
    where 
        buildField m = Field {
            _fAccessFlag = mAccessFlags m,
            _fName = memName m,
            _fDesc = memDesc m
        }

-- 将MemberInfo(来自于ClassFile)转换成Method
newMethod :: [MemberInfo] -> [Method]
newMethod [] = []
newMethod (x:xs) = buildMethod x : newMethod xs
    where
        buildMethod :: MemberInfo -> Method
        buildMethod m = Method {
            _mAccessFlag = mAccessFlags m,
            _mName = memName m,
            _mDesc = memDesc m,
            _maxStack = maybe 0 maxStack $ codeAttr (memberAttributes m),
            _maxLocalTable = maybe 0 maxLocals $ codeAttr (memberAttributes m),
            _code = maybe [] code $ codeAttr (memberAttributes m)
        }
        -- 查找CodeAttribute
        codeAttr :: [AttributeInfo] -> Maybe AttributeInfo
        codeAttr = findCodeAttribute'