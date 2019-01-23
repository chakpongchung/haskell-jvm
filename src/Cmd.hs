module Cmd
(
Cmd(..)
,cmd
)where

import Control.Applicative
import Data.Semigroup ((<>))
import Options.Applicative


data Cmd = Cmd {
  classpath :: String
 ,xjre :: String
 ,clazz :: String
 ,args :: [String]
} deriving(Show)

cmd :: Parser Cmd
cmd = Cmd 
    <$> strOption (long "classpath" <> short 'c' <> metavar "路径" <> help "指定查找用户类文件和注释处理程序的位置")
    <*> strOption (long "xjre" <> metavar "路径" <> help "Jre目录位置")
    <*> strOption (long "source" <> metavar "主类" <> help "源文件")
    <*> some (argument str (metavar "PARAMS..."))