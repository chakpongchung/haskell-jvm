module Runtime.Heap.AccessFlags (
        isPublic,
        AccessFlags(..),
        accessFlag
    ) where

import Data.Bits
import Data.Word

type AccessFlag = Word16

data AccessFlags = 
    ACC_PUBLIC
    | ACC_PRIVATE
    | ACC_PROTECTED
    | ACC_STATIC
    | ACC_FINAL
    | ACC_SUPER
    | ACC_SYNCHRONIZED
    | ACC_VOLATILE
    | ACC_BRIDGE
    | ACC_TRANSIENT
    | ACC_VARARGS
    | ACC_NATIVE
    | ACC_INTERFACE
    | ACC_ABSTRACT
    | ACC_STRICT
    | ACC_SYNTHETIC
    | ACC_ANNOTATION
    | ACC_ENUM
  
accessFlag :: AccessFlags -> Word16
accessFlag af = 
    case af of 
        ACC_PUBLIC          -> 0x0001       -- ^ public 修饰符
        ACC_PRIVATE         -> 0x0002       -- ^ private 修饰符
        ACC_PROTECTED       -> 0x0004       -- ^ protected 修饰符
        ACC_STATIC          -> 0x0008       -- ^ static 修饰符
        ACC_FINAL           -> 0x0010       -- ^ final 修饰符
        ACC_SUPER           -> 0x0020       
        ACC_SYNCHRONIZED    -> 0x0020       -- ^ synchronized 修饰符
        ACC_VOLATILE        -> 0x0040       -- ^ volatile 修饰符 
        ACC_BRIDGE          -> 0x0040       -- ^ bridge 桥接方法 
        ACC_TRANSIENT       -> 0x0080
        ACC_VARARGS         -> 0x0080
        ACC_NATIVE          -> 0x0100
        ACC_INTERFACE       -> 0x0200       
        ACC_ABSTRACT        -> 0x0400
        ACC_STRICT          -> 0x0800
        ACC_SYNTHETIC       -> 0x1000       -- ^ 用来绕开语言限制，对private级别的字段和类进行访问，例如：内部类的私有变量被外部类引用时，编译器会生成一个access方法,外部类访问的就是该方法
        ACC_ANNOTATION      -> 0x2000           
        ACC_ENUM            -> 0x4000

-- 是否有public修饰符        
isPublic :: AccessFlag -> Bool
isPublic af = 0 /= (af .&. accessFlag ACC_PUBLIC)

isFinal :: AccessFlag -> Bool
isFinal af = 0 /= (af .&. accessFlag ACC_FINAL)

isSuper :: AccessFlag -> Bool
isSuper af = 0 /= (af .&. accessFlag ACC_SUPER)

isInterface :: AccessFlag -> Bool
isInterface af = 0 /= (af .&. accessFlag ACC_INTERFACE)

isAbstract :: AccessFlag -> Bool
isAbstract af = 0 /= (af .&. accessFlag ACC_ABSTRACT)

isAnnotation :: AccessFlag -> Bool
isAnnotation af = 0 /= (af .&. accessFlag ACC_ANNOTATION)

isEnum :: AccessFlag -> Bool    
isEnum af = 0 /= (af .&. accessFlag ACC_ENUM)

isSynthetic :: AccessFlag -> Bool
isSynthetic af = 0 /= (af .&. accessFlag ACC_SYNTHETIC)