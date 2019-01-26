module Classpath.ClassPath(
 ClassPath
 ,makeClassPath
 ,readClass
) where

import Classpath.Entry
import CommandLine
import Data.Monoid
import Data.Maybe
import GHC.IO
import System.Directory
import System.Environment

data ClassPath = ClassPath {
 boot :: !Entry,
 ext :: !Entry,
 user :: !Entry
} deriving (Show)



currentJrePath :: String
currentJrePath = "./jre"

currentClassPath :: String
currentClassPath = "."

javaHomeEnv :: String 
javaHomeEnv = "JAVA_HOME"

makeClassPath :: CommandLine -> IO ClassPath
makeClassPath cmd = do
    currentAbsJre <- findJrePathByCurrentDir
    javaHome <- findJrePathByJavaHome

    let makeJrePath = fromMaybe (error "not't found jre path!") . getFirst 
            $ findJrePathByJreOption 
            `mappend` First currentAbsJre
            `mappend` First ((++) <$> javaHome <*> Just "jre")

    bootStrapPath <- absPath $ makeJrePath ++ "/lib"
    extensionPath <- absPath $ makeJrePath ++ "/lib/ext"

    bootEntry <- newWildcardEntry bootStrapPath
    extEntry <- newWildcardEntry extensionPath
    cpEntry <- newEntry findClassPath

    return $! ClassPath {
        boot = bootEntry, 
        ext = extEntry,
        user = cpEntry
    } 
    where 
        findJrePathByJreOption = First $ jre cmd

        findJrePathByCurrentDir = do
            checkJre <- canonicalizePath currentJrePath
            exist <- doesPathExist checkJre
            if exist then return (Just currentJrePath) else return Nothing

        findJrePathByJavaHome = lookupEnv javaHomeEnv
                    
        findClassPath = fromMaybe currentClassPath $ classpath cmd 
              
        
-- ClassName java.lang.Object
readClass :: ClassPath -> ClassName -> IO ClassContent
readClass cp cn = do
        bootContent <- loadClass (boot cp) cn
        extContent <- loadClass (ext cp) cn
        userContent <- loadClass (user cp) cn
        let content = fromMaybe (error "class not found") . getFirst 
                $ (First bootContent) 
                `mappend` (First extContent) 
                `mappend` (First userContent)
        putStrLn "---------readClass------------"      
        putStrLn content
        return content