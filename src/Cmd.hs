module Cmd
(
Cmd(..)
,version
,cmdParser


)where

import Control.Applicative
import Data.Semigroup ((<>))
import Options.Applicative
import Options.Applicative.Builder


data Cmd = Cmd {
   classpath :: Maybe String 
   ,jre :: Maybe String
   ,args :: [String]
  } deriving(Show,Read)


version :: Parser (Cmd -> Cmd)
version = infoOption "0.1.0.0" (long "version" <> short 'v' <> help "查看版本号")

classpathParser :: Parser String
classpathParser = strOption (long "classpath" <> short 'c' <> metavar "path" <> help "指定查找用户类文件和注释处理程序的位置")

jreParser :: Parser String
jreParser = strOption (long "jre" <> short 'j' <> metavar "路径" <> help "jre目录路径")

cmdParser :: Parser Cmd
cmdParser = Cmd 
    <$> optional classpathParser
    <*> optional jreParser 
    <*> some (strArgument (metavar "[MainClass]Params..." <> help "主类以及参数列表"))

