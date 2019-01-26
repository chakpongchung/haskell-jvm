module CommandLine
(
CommandLine(..)
,commandParser
,getMainClass
,getMainClassParam
)where

import Control.Applicative
import Data.Semigroup ((<>))
import Options.Applicative
import Options.Applicative.Builder
import qualified Data.Text as T

data CommandLine = CommandLine {
   classpath :: Maybe String 
   ,jre :: Maybe String
   ,args :: [String]
  } deriving(Show,Read)

getMainClass :: [String] -> String
getMainClass [] = error "missing main class..."
getMainClass (x:xs) = 
    T.unpack replace ++ ".class"
    where replace = T.replace (T.pack ".") (T.pack "/") (T.pack x)

getMainClassParam :: [String] -> [String]
getMainClassParam  = tail

versionParser :: Parser (CommandLine -> CommandLine)
versionParser = infoOption "0.1.0.0" (long "version" <> short 'v' <> help "查看版本号")

classpathParser :: Parser String
classpathParser = strOption (long "classpath" <> short 'c' <> metavar "path" <> help "指定查找用户类文件和注释处理程序的位置")

jreParser :: Parser String
jreParser = strOption (long "jre" <> short 'j' <> metavar "路径" <> help "jre目录路径")

optionParser :: Parser CommandLine
optionParser = CommandLine 
    <$> optional classpathParser
    <*> optional jreParser 
    <*> some (strArgument (metavar "MainClass Params..." <> help "主类以及参数列表, e.g: java.lang.Object"))

commandParser :: Parser CommandLine
commandParser = optionParser <**> versionParser