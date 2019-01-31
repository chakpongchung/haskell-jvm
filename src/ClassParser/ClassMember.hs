module ClassParser.ClassMember (
    readMemberInfo
    ,MemberInfo(..)
) where 

import Common
import ClassParser.ClassAttributes
import ClassParser.ConstantPool
import Data.Word
import Control.Monad
import Data.Binary.Get

data MemberInfo = MemberInfo {
    accessFlags :: Word16,
    memNameIndex :: PoolIndex,
    memDescIndex :: PoolIndex,
    memberAttributes :: [AttributeInfo]
} deriving (Show)

check :: Word16 -> Bool
check n 
    | n == 0 = error $ "first: " ++ (show $ fromEnum n)
    | otherwise =  error $ "second: " ++ (show $ fromEnum n)

tag :: MemberInfo -> Bool
tag m
 | True = error ("first: " ++ (show $ m))
 | otherwise =  error ("second: " ++ (show $ m))

readMemberInfo :: ConstantPool -> Get [MemberInfo]
readMemberInfo pool = do
    memberNum <- getWord16be
    -- guard $ check memberNum
    newMemberInfo' pool $ fromEnum memberNum

newMemberInfo' :: ConstantPool -> Int -> Get [MemberInfo]
newMemberInfo' _ 0 = return []
newMemberInfo' pool n = do
    memberInfo <- newMemberInfo pool

    -- guard $ tag memberInfo
    
    next <- newMemberInfo' pool $ n - 1
    return $ memberInfo : next

newMemberInfo :: ConstantPool -> Get MemberInfo
newMemberInfo pool = do
   accessFlags <- getWord16be
   nameIndex <- getWord16be
   descIndex <- getWord16be
   -- read attribute
   attributes <- readAttributes pool

   return $ MemberInfo {
        accessFlags = accessFlags,
        memNameIndex = nameIndex,
        memDescIndex = descIndex,
        memberAttributes = attributes
   }

