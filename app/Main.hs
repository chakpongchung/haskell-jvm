import System.Environment
import Cmd
import Classpath.ClassPath
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe

main :: IO ()
main = dispatch =<< execParser opts
 where
     opts = info (cmdParser <**> version <**> helper) ( fullDesc <> progDesc "a JVM implements by Haskell" <> header "---------使用说明---------" )

dispatch :: Cmd -> IO ()
dispatch c = 
    case args c of 
        ("":xs) -> error "MainClass缺失"
        _       -> startJVM c

startJVM :: Cmd -> IO()
startJVM c = do
    putStrLn . show $ c
    putStrLn "开始启动JVM..."
    return ()

{-
  解析类路径
  ClassPath.parse(cmd.jre,cmd.cp)
  读取class文件
  data = ClassPath.readClass(cmd.clazz)
  输出文件数据
  print data
-}