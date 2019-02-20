{-
  运行时常量池，主要用来存放：字面量(literal) 和 符号引用(symbolic reference)
    字面量：
        整数
        浮点数
        字符串字面量
    符号引用：
        类符号引用
        方法符号引用
        字段符号引用
        接口方法符号引用    
-}
module Runtime.Heap.RuntimeConstantPool (

) where

import Runtime.Heap.JavaClass
import ClassParser.ConstantPool

-- 运行时常量池索引
type ConstantPoolIndex = Word

-- 运行时常量池中的常量
data Constant = JInt Int32 | JFloat Float | JLong Int64 | JDouble Double 
    | JString String
    | JClass 
    | JField
    | JMethod
    | JInterface

-- 类符号引用
data ClassSymbolRef = ClassSymbolRef {
    className :: String
    -- constantPool :: RuntimeConstantPool,
    -- javaClass :: JavaClass
}

data MemberSymbolRef = 
    FieldSymbolRef {                -- ^ 字段符号引用
        fClassName :: String,
        fName :: String,            -- ^ java虚拟机允许同一个类中出现多个同名字段，只要字段类型不同就行，而java语言规范则不允许
        fDesc :: String
        -- field :: Field
    } 
    | MethodSymbolRef {             -- ^ 方法符号引用
        mClassName :: String,
        mName :: String,
        mDesc :: String
        -- method :: Method
    }
    | InterfaceMethodSymbolRef {    -- ^ 接口方法符号引用
        iClassName :: String,
        iName :: String,
        iDesc :: String
        -- method :: Method
    }

-- 运行时常量池
data RuntimeConstantPool = RuntimeConstantPool {
    javaClass :: JavaClass,
    constants :: [Constant]
}

-- 创建运行时常量池
newRuntimeConstantPool :: JavaClass -> ConstantPool -> RuntimeConstantPool
newRuntimeConstantPool c p = 
        RuntimeConstantPool {
            javaClass = c,
            constants = newRuntimeConstantPool p p
        }
    where 
        poolSize = constantPool p

-- 创建常量池      
newRuntimeConstantPool :: ConstantPool -> ConstantPool -> [Constant]
newRuntimeConstantPool [] _ = []
newRuntimeConstantPool (x:xs) pool = 
    case newRuntimeConstant x pool of
        -- long 和 double 需要占用两个位置，这里临时通过占位符解决
        JLong i64 -> JLong i64 : JLong 0 : newRuntimeConstantPool xs pool
        JDouble d -> JDouble d : JDouble 0 : newRuntimeConstantPool xs pool
        _           -> _ : newRuntimeConstantPool xs pool

-- 创建常量
-- 解析常量池时只保存了索引(递归生成的常量池，见: ClasParser.newConstantInfo)，没有保存实际的字符串信息，因此这里使用常量池索引去pool中查询字符串信息       
newRuntimeConstant :: ConstantInfo -> ConstantPool -> Constant
newRuntimeConstant ci pool =
    case ci of
        ConstantIntegerInfo i32 -> JInt i32
        ConstantFloatInfo f -> JFloat f
        ConstantLongInfo l -> JLong l
        ConstantStringInfo index -> JString (getUtf8 index pool)

-- 创建类符号引用        
newClassSymbolRef :: ConstantInfo -> ConstantPool -> ClassSymbolRef
newClassSymbolRef (ConstantClassInfo nameIndex) c = ClassSymbolRef {
    className = getUtf8 nameIndex c
} 

-- 创建符号引用
newMemberSymbolRef :: ConstantInfo -> ConstantPool -> MemberSymbolRef
newMemberSymbolRef c p = 
    case c of
        ConstantFieldRefInfo classIndex nameAndTypeIndex    -> 
            FieldSymbolRef {
                fClassName = getUtf8 classIndex p,
                fName = fieldName,
                fDesc = fieldDesc
            }
        ConstantMethodRefInfo classIndex nameAndTypeIndex   ->
            MethodSymbolRef {
                mClassName = getUtf8 classIndex p,
                mName = fieldName,
                mDesc = fieldDesc
            }    
        ConstantInterfaceMethodRefInfo classIndex nameAndTypeIndex ->
            InterfaceMethodSymbolRef {
                iClassName = getUtf8 classIndex p,
                iName = fieldName,
                iDesc = fieldDesc
            }
    where 
        (fieldName,fieldDesc) = getNameAndDesc nameAndTypeIndex p

-- 根据索引从常量池中查询常量
getConstantByIndex :: ConstantPoolIndex -> RuntimeConstantPool -> Constant
getConstantByIndex i pool
    | index < 0 = error $ "not found Constant at index: " ++ index ++ ",current Constant-Pool-Size: " ++ poolSize
    | index >= poolSize = error $ "not found Constant at index: " ++ index ++ ",current Constant-Pool-Size: " ++ poolSize
    | otherwise = constants pool !! index
    where
        index = i - 1
        poolSize = length pool


