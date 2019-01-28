{-# LANGUAGE ScopedTypeVariables #-}
module Classpath.ClassEntry(
 ClassEntry(..)
 ,newClassEntry
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
import Codec.Archive.Zip as Z
import qualified Data.Map as M
import Control.Exception.Base
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad
import Control.Applicative
import Data.Maybe
import qualified Data.ByteString.Lazy as L

data ClassEntry = 
    DirEntry String | 
    ZipEntry String | 
    CompositeEntry [ClassEntry] | 
    WildcardEntry [ClassEntry] deriving (Show)

wild :: String
wild = "*"

type ClassName = String
type ClassContent = L.ByteString

zipJarList :: [String]
zipJarList = ["zip","ZIP"] ++ jarSuffix

jarSuffix :: [String]
jarSuffix = ["JAR","jar"]

newClassEntry :: FilePath  -> IO ClassEntry
newClassEntry path
  | searchPathSeparator `elem` path = newCompositeEntry path
  | wild `isSuffixOf` path = newWildcardEntry path
  | any (`isSuffixOf` path) zipJarList = newZipJarEntry path
  | otherwise = newDirEntry path

newDirEntry :: FilePath -> IO ClassEntry
newDirEntry path = do
    abs <- absPath path
    -- print "---------newDirEntry-----------"
    return $ DirEntry abs

newZipJarEntry :: FilePath -> IO ClassEntry
newZipJarEntry path = do
    abs <- absPath path
    -- print "---------newZipJarEntry-----------"
    return $ ZipEntry abs

-- FilePath:  /Library/Java/JavaVirtualMachines/jdk1.8.0_191.jdk/Contents/Home/jre/lib
newWildcardEntry :: FilePath -> IO ClassEntry
newWildcardEntry basePath = do
        filePathList <- listDirectory basePath
        let jarList = map (\x -> basePath ++ "/" ++ x) $ filter (\path -> any (`isSuffixOf` path) jarSuffix) filePathList
        -- putStrLn "----------newWildcardEntry----------------"
        -- print jarList
        zipEntryList <- mapM newZipJarEntry jarList
        return $ CompositeEntry zipEntryList
    where
        -- /lib/ext/*
        wildPath = basePath ++ "/*"

newCompositeEntry :: FilePath -> IO ClassEntry    
newCompositeEntry basePath = do
        print $ "~~~~~~~newCompositeEntry~~~~~~~~~~" ++ basePath
        let pathList = splitSearchPath basePath
        entryList <- mapM newClassEntry pathList
        return $ CompositeEntry entryList
    
absPath :: FilePath -> IO String
absPath path = do
    absPath <- canonicalizePath path
    exist <- doesPathExist absPath
    if exist 
        then return absPath 
        else error $ "dir [" ++ path ++ "] is not exist"

loadClass :: ClassEntry -> ClassName -> MaybeT IO ClassContent
loadClass (DirEntry absPath) className = 
    MaybeT $ catch 
        (do
            print $ "~~~~~~~DirEntry~~~~~~~~~~" ++ absPath ++ " | " ++ className
            handler <- openBinaryFile (fileFullPath absPath className) ReadMode
            Just <$> L.hGetContents handler 
        ) 
        (\(e :: SomeException) -> mzero)

-- absPath:  /opt/test.zip
loadClass (ZipEntry absPath) className = do
    r <-  lift $ catch
        (do
            print $ "~~~~~~~ZipEntry~~~~~~~~~~" ++ absPath ++ " | " ++ className
            zipFile <- L.readFile absPath
            -- print $ filesInArchive .  toArchive $ zipFile
            let a = toArchiveOrFail zipFile
            case a of
                Right archive   ->  return $ Just archive
                _               ->  mzero 
        )
        (\(e :: SomeException) -> mzero)

    guard $ isJust r
    
    case findEntryByPath className (fromJust r) of 
        Just entry  ->  MaybeT $ return $ Just $ fromEntry entry
        Nothing     ->  mzero   

loadClass (CompositeEntry xs) className = 
    case xs of 
        []  -> MaybeT $ return Nothing
        (x:xs)  -> loadClass x className <|> loadClass (CompositeEntry xs) className

loadClass (WildcardEntry xs) className = 
    case xs of 
        []  -> MaybeT $ return Nothing
        (x:xs)  -> loadClass x className <|> loadClass (WildcardEntry xs) className

fileFullPath :: FilePath -> ClassName -> FilePath
fileFullPath absPath name = absPath ++ "/" ++ name

