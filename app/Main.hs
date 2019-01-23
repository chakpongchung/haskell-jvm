import System.Environment
import Cmd
import Classpath.ClassPath
import Options.Applicative
import Data.Semigroup ((<>))

main :: IO ()
main = parse =<< execParser opts
 where
     opts = info ( cmd <**> helper) ( fullDesc <> progDesc "a JVM implements by Haskell" <> header "---------使用说明---------" )
parse :: Cmd -> IO ()
parse c = case c of 
    Cmd cp xj ca arg -> putStrLn cp

-- dispatch :: Cmd -> IO()
-- dispatch c
--   | version c = putStrLn printVersion
--   | help c = putStrLn printHelp
--   | otherwise = startJVM c


-- startJVM :: Cmd -> IO()
-- startJVM cmd = print cmd

{-
  解析类路径
  ClassPath.parse(cmd.jre,cmd.cp)
  读取class文件
  data = ClassPath.readClass(cmd.clazz)
  输出文件数据
  print data
-}