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
import Control.Monad.State.Lazy
import Runtime.Thread.JvmThread
import Runtime.Thread.JvmStack
import Runtime.Thread.JvmStackFrame
import Runtime.Thread.LocalVariableTable
import Runtime.Thread.OperandStack

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

    let jvmStackFrame = newJvmStackFrame 10 10
    runStateT testLocalVariableTable jvmStackFrame
    runStateT testOperandStack jvmStackFrame
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


testLocalVariableTable :: StateT JvmStackFrame IO ()
testLocalVariableTable = do
    lift $ print "---------------------set LocalVariableTable--------------------------"
    setInt 0 100
    setInt 1 (-100)
    setLong 2 2997924580
    setLong 4 (-2997924580)
    setFloat 6 3.1415926
    setDouble 7 2.71828182845
    setReference 9 Object{name = "object"}

    jsf <- get
    let table = localtable jsf
    lift $ print table
    lift $ print "---------------------get LocalVariableTable--------------------------"

    v0 <- getInt 0
    v1 <- getInt 1
    v2 <- getLong 2
    v4 <- getLong 4
    v6 <- getFloat 6
    v7 <- getDouble 7
    v9 <- getReference 9

    lift $ print $ "v0: " ++ show v0
    lift $ print $ "v1: " ++ show v1
    lift $ print $ "v2: " ++ show v2
    lift $ print $ "v4: " ++ show v4
    lift $ print $ "v6: " ++ show v6
    lift $ print $ "v7: " ++ show v7
    lift $ print $ "v9: " ++ show v9
    return ()

testOperandStack :: StateT JvmStackFrame IO ()
testOperandStack = do
    lift $ print "---------------------set OperandStack--------------------------"
    pushInt 100
    pushInt (-100)
    pushLong 2997924580
    pushLong (-2997924580)
    pushFloat 3.1415926
    pushDouble 2.71828182845
    pushReference Object{name = "object"}

    jsf <- get
    lift $ print jsf
    lift $ print "---------------------get OperandStack--------------------------"
    v1 <- popReference
    v2 <- popDouble
    v3 <- popFloat
    v4 <- popLong
    v5 <- popLong
    v6 <- popInt
    v7 <- popInt

    lift $ print $ "v0: " ++ show v1
    lift $ print $ "v1: " ++ show v2
    lift $ print $ "v2: " ++ show v3
    lift $ print $ "v4: " ++ show v4
    lift $ print $ "v6: " ++ show v5
    lift $ print $ "v7: " ++ show v6
    lift $ print $ "v8: " ++ show v7 
    return ()