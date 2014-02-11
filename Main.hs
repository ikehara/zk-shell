
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- import Control.Exception
import Data.List.Split
import Control.Concurrent.STM
import System.Console.Haskeline
import System.Console.CmdArgs
import System.Environment
import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class
import qualified Database.Zookeeper as Z

-- Multi-mode options
-- See http://zuttobenkyou.wordpress.com/2011/04/19/haskell-using-cmdargs-single-and-multi-mode/
data Options =
    Get {
       path :: String
     }
  | Set {
       path :: String
     , content :: String
     }
  | Stat {
     }
  deriving (Data, Typeable, Show, Eq)
 
get :: Options
get = Get {
    path = def &= typ "PATH" &= argPos 0
  } &= details  [ "Examples:"
                , "get /foo/bar"
                ]
 
set :: Options
set = Set { 
    path = def &= typ "PATH" &= argPos 0
  , content = def &= typ "CONTENT" &= argPos 1
  } &= details  [ "Examples:"
                , "set /foo/bar baz"
                ]
 
stat :: Options
stat = Stat {
  } &= details  [ "Examples:"
                , "stat"
                ]

commandModes :: Mode (CmdArgs Options)
commandModes = cmdArgsMode $ modes [get, set, stat]
  &= verbosityArgs [explicit, name "Verbose", name "V"] []
  &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
  &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
  &= help _PROGRAM_ABOUT
  &= helpArg [explicit, name "help", name "h"]
  &= program _PROGRAM_NAME
 
_PROGRAM_NAME = "zksh"
_PROGRAM_VERSION = "0.0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "a shell for zookeeper"
_COPYRIGHT = "(C) Kiyoshi IKEHARA 2014"

main :: IO ()
main = do
  args <- getArgs
  if null args then runInputT defaultSettings loop else do
    opts <- cmdArgsRun commandModes
    optionHandler opts
  where 
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "% "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          (liftIO $ execute $ splitOn " " input) `catch` (\(e :: SomeException) -> return ())
          loop

execute args = do
  opts <- withArgs (if null args then ["--help"] else args) $ cmdArgsRun commandModes
  optionHandler opts

optionHandler :: Options -> IO ()
optionHandler opts@Get {}  = do
    exec opts
optionHandler opts@Set {}  = do
    exec opts
optionHandler opts = do
    exec opts
 
exec :: Options -> IO ()
exec opts@Get {} = do
  putStrLn $ "get " ++ path opts
exec opts@Set {} = do
  putStrLn $ "set " ++ path opts ++ " " ++ show (content opts)
exec opts@Stat {} = do
  putStrLn $ "stat"
