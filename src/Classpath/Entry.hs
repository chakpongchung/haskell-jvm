{-# LANGUAGE ScopedTypeVariables #-}
module Classpath.Entry(
 Entry(..)
 ,newEntry
 ,newWildcardEntry
 ,absPath
 ,ClassName
 ,ClassContent
 ,loadClass
) where

import Data.List
import GHC.IO
import System.IO
import System.Directory
import System.FilePath
import qualified Control.Exception as E
import Codec.Archive.Zip
import qualified Data.Map as M
import Control.Exception.Base
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad
import Control.Applicative

data Entry = 
    DirEntry String | 
    ZipEntry String | 
    CompositeEntry [Entry] | 
    WildcardEntry [Entry] deriving (Show)

wild :: String
wild = "*"


type ClassName = String
type ClassContent = String 

zipJarList :: [String]
zipJarList = ["zip","ZIP"] ++ jarList

jarList :: [String]
jarList = ["JAR","jar"]

newEntry :: FilePath  -> IO Entry
newEntry path
  | searchPathSeparator `elem` path = newCompositeEntry path
  | wild `isSuffixOf` path = newWildcardEntry path
  | any (`isSuffixOf` path) zipJarList = newZipJarEntry path
  | otherwise = newDirEntry path

newDirEntry :: FilePath -> IO Entry
newDirEntry path = do
    abs <- absPath path
    print "---------newDirEntry-----------"
    return $ DirEntry abs

newZipJarEntry :: FilePath -> IO Entry
newZipJarEntry path = do
    abs <- absPath path
    print "---------newZipJarEntry-----------"
    return $ ZipEntry abs

newWildcardEntry :: FilePath -> IO Entry
newWildcardEntry basePath = do
        filePathList <- listDirectory basePath
        jarList <- findFilesWith (\path -> return $ any (`isSuffixOf` path) jarList) filePathList ""
        putStrLn "----------newWildcardEntry----------------"
        putStrLn . show $ jarList
        zipEntry <- mapM newZipJarEntry jarList
        return $ WildcardEntry zipEntry
    where
        -- /lib/ext/*
        wildPath = basePath ++ "/*"

newCompositeEntry :: FilePath -> IO Entry    
newCompositeEntry basePath = do
        let pathList = splitSearchPath basePath
        putStrLn "----------newCompositeEntry----------------"
        putStrLn . show $ pathList
        entryList <- mapM newEntry pathList
        return $ CompositeEntry entryList
    
absPath :: FilePath -> IO String
absPath path = do
    absPath <- canonicalizePath path
    exist <- doesPathExist absPath
    if exist 
        then return absPath 
        else error $ "dir [" ++ path ++ "] is not exist"

loadClass :: Entry -> ClassName -> MaybeT IO String
loadClass (DirEntry absPath) className = 
    MaybeT $ catch 
        (do
            handler <- openBinaryFile (fileFullPath absPath className) ReadMode
            Just <$> hGetContents handler 
        ) 
        (\(e :: SomeException) -> mzero)
loadClass _ _ = MaybeT $ return Nothing
-- loadClass (ZipEntry String) className = 

-- loadClass (CompositeEntry [Entry]) ClassName = 

-- loadClass (WildcardEntry [Entry]) ClassName = 


fileFullPath :: FilePath -> ClassName -> FilePath
fileFullPath absPath name = absPath ++ "/" ++ name

