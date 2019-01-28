import System.Environment
import CommandLine
import Classpath.ClassPath
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe

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
    print classPath
    readClass classPath . getMainClass $ args c
    return ()