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
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad
import Control.Applicative

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

    maybeJrePath <- runMaybeT $ findJreByJreOption cmd <|> findJreByCurrentDir <|> findJreByJavaHome
    
    let jrePath = fromMaybe (error "not't found jre path!") maybeJrePath

    bootStrapPath <- absPath $ jrePath ++ "/lib"
    extensionPath <- absPath $ jrePath ++ "/lib/ext"

    bootEntry <- newWildcardEntry bootStrapPath
    extEntry <- newWildcardEntry extensionPath
    cpEntry <- newEntry findClassPath

    return $! ClassPath {
        boot = bootEntry, 
        ext = extEntry,
        user = cpEntry
    } 
    where                     
        findClassPath = fromMaybe currentClassPath $ classpath cmd 
              
findJreByJreOption :: CommandLine -> MaybeT IO String 
findJreByJreOption cmd = MaybeT (return $ jre cmd)

findJreByCurrentDir :: MaybeT IO String
findJreByCurrentDir = do
    checkJre <- lift $ canonicalizePath currentJrePath
    exist <- lift $ doesPathExist checkJre
    guard exist
    return currentJrePath

findJreByJavaHome :: MaybeT IO String
findJreByJavaHome = do
    javaHome <- lift $ lookupEnv javaHomeEnv
    MaybeT (return ((++) <$> javaHome <*> return "jre"))

-- ClassName java.lang.Object
readClass :: ClassPath -> ClassName -> IO ClassContent
readClass cp cn = do
    classContent <- runMaybeT $ loadClass (boot cp) cn <|> loadClass (ext cp) cn <|> loadClass (user cp) cn
    let content = fromMaybe (error "read class error, class not found") classContent
    putStrLn "---------readClass------------"      
    putStrLn content
    return content