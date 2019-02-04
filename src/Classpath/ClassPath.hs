module ClassPath.ClassPath(
 ClassPath
 ,makeClassPath
 ,readClass
) where

import ClassPath.ClassEntry
import ClassPath.CommandLine
import Common
import Data.Monoid
import Data.Maybe
import GHC.IO
import System.Directory
import System.Environment
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad
import Data.Word
import Control.Applicative
import qualified Data.ByteString.Lazy as L

data ClassPath = ClassPath {
 boot :: !ClassEntry,
 ext :: !ClassEntry,
 user :: !ClassEntry
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

    print $ "bootStrapPath: " ++ bootStrapPath
    print $ "extensionPath: " ++ extensionPath
    print $ "userClassPath: " ++ findClassPath

    bootEntry <- newWildcardEntry bootStrapPath
    extEntry <- newWildcardEntry extensionPath
    cpEntry <- newClassEntry findClassPath

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
    -- join $JAVA_HOME and "jre"
    MaybeT (return ((++) <$> javaHome <*> return "jre"))

-- ClassName: java.lang.Object -> java/lang/Object.class
readClass :: ClassPath -> ClassName -> IO L.ByteString
readClass cp cn = do
    classContent <- runMaybeT $ loadClass (boot cp) cn <|> loadClass (ext cp) cn <|> loadClass (user cp) cn
    let errMsg = "read class error, class["++cn++"] not found"
    let content = fromMaybe (error errMsg) classContent
    putStrLn "---------readClass------------"      
    -- print content
    -- print $ L.unpack content
    return content