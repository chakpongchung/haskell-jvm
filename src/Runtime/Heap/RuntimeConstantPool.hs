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

-- TODO 创建类符号引用        
-- newClassRefInfo ::         


-- 根据索引从常量池中查询常量
getConstantByIndex :: ConstantPoolIndex -> RuntimeConstantPool -> Constant
getConstantByIndex i pool
    | index < 0 = error $ "not found Constant at index: " ++ index ++ ",current Constant-Pool-Size: " ++ poolSize
    | index >= poolSize = error $ "not found Constant at index: " ++ index ++ ",current Constant-Pool-Size: " ++ poolSize
    | otherwise = constants pool !! index
    where
        index = i - 1
        poolSize = length pool


