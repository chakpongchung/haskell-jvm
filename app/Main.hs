import System.Environment
import ClassPath.CommandLine
import Common
import ClassPath.ClassPath
import ClassParser.ClassMember
import ClassParser.ClassReader
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.Binary.Get
import Runtime.Thread.JvmThread
import Control.Monad.State.Lazy

main :: IO ()
main = dispatch =<< execParser opts
 where
     opts = info (commandParser <**> helper) ( fullDesc <> progDesc "a JVM implements by Haskell" <> header "---------使用说明---------" )

dispatch :: CommandLine -> IO ()
dispatch c =
    case args c of
        ("":xs) -> error "missing main class, e.g: java.lang.Object"
        _       -> startJVM c

startJVM :: CommandLine -> IO()
startJVM c = do
    print c
    putStrLn "start haskell-jvm..."
    classPath <- makeClassPath c
    -- print classPath
    classContent <- readClass classPath . getMainClass $ args c

    print "--------------Start Parser ClassFile-------------"
    let classFile = runGet parserClass classContent
    print "--------------Print Parser ClassFile--------------"
    printClassFile classFile
    let thread = newJvmThread 4
    print "============================="
    runStateT testThread thread >> return ()
    return ()

printClassFile :: ClassFile -> IO ()
printClassFile cf = do
    print cf
    putStrLn $ "version: " ++ show (majorVersion cf) ++ "." ++ show (minorVersion cf)
    putStrLn $ "constants count: " ++ show (length $ constantPool cf)
    putStrLn $ "access flags: " ++ show (accessFlags cf)
    putStrLn $ "this class: " ++ show (thisClassName cf)
    putStrLn $ "super class: " ++ show (superClassName cf)
    putStrLn $ "interfaces: " ++ show (interfaces cf)
    putStrLn $ "interfaceName: " ++ show (interNames cf)
    putStrLn $ "fileds count:" ++ show (length $ fields cf)
    mapM_ (\m -> putStrLn $ "\t" ++ memName m) $ fields cf

    putStrLn $ "methods count:" ++ show (length $ methods cf)
    mapM_ (\m -> putStrLn $ "\t" ++ memName m) $ methods cf

    return ()