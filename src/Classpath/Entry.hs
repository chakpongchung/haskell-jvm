module Classpath.Entry(
 Entry
 ,newEntry
) where

import Data.List

data Entry = DirEntry String | ZipEntry String | CompositeEntry String | WildcardEntry String

type Path = String
type AbsPath = String
fileSeparator :: Char
fileSeparator = ';'

wild :: String
wild = "*"

zipList :: [String]
zipList = ["zip","ZIP","JAR","jar"]

newEntry :: Path -> AbsPath -> Entry
newEntry path absPath
  | fileSeparator `elem` path = CompositeEntry absPath
  | wild `isSuffixOf` path = WildcardEntry absPath
  | any (`isSuffixOf` path) zipList = ZipEntry absPath
  | otherwise = DirEntry absPath
  
{- 待实现
class Parser a where
 --path: java/lang/Object.class
 readClass :: a -> Path -> a
 string :: a -> String -> String
instance Parser Entry where
 readClass DirEntry path = 
 -}