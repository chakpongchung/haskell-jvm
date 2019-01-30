import System.Environment
import CommandLine
import Common
import Classpath.ClassPath
import ClassParser.ClassReader
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.Binary.Get

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

    print "----------------------"
    print $ runGet parserClass classContent

    return ()