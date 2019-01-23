module Classpath.ClassPath(
 ClassPath
) where

import Classpath.Entry

data ClassPath = ClassPath {
 boot :: Entry,
 ext :: Entry,
 user :: Entry
}

{- 待实现
type jreOption = String
type cpOption = String
type className = String
parse :: jreOption -> cpOption -> ClassPath
parse jre cp = ClassPath{}
-- className = java/lang/Object.class
readClass :: className -> ClassPath
-}