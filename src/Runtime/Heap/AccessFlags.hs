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
        ACC_PUBLIC          -> 0x0001
        ACC_PRIVATE         -> 0x0002
        ACC_PROTECTED       -> 0x0004
        ACC_STATIC          -> 0x0008
        ACC_FINAL           -> 0x0010
        ACC_SUPER           -> 0x0020
        ACC_SYNCHRONIZED    -> 0x0020
        ACC_VOLATILE        -> 0x0040
        ACC_BRIDGE          -> 0x0040
        ACC_TRANSIENT       -> 0x0080
        ACC_VARARGS         -> 0x0080
        ACC_NATIVE          -> 0x0100
        ACC_INTERFACE       -> 0x0200
        ACC_ABSTRACT        -> 0x0400
        ACC_STRICT          -> 0x0800
        ACC_SYNTHETIC       -> 0x1000
        ACC_ANNOTATION      -> 0x2000
        ACC_ENUM            -> 0x4000

isPublic :: AccessFlag -> Bool
isPublic af = 0 /= (af .&. accessFlag ACC_PUBLIC)
    
    