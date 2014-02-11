
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
import System.IO
import Control.Monad
import Text.Printf

data Shell = Shell {
    sZookeeper :: Z.Zookeeper
  , sAclsVar :: TVar Z.AclList
  }

-- Multi-mode options
-- See http://zuttobenkyou.wordpress.com/2011/04/19/haskell-using-cmdargs-single-and-multi-mode/
data Options =
    Ls {
       path :: String
     , long :: Bool
     }
  | Get {
       path :: String
     }
  | Set {
       path :: String
     , content :: String
     }
  | Create {
       path :: String
     , content :: String
     }
  | Touch {
       path :: String
     }
  | Rm {
       path :: String
     , recursive :: Bool
     }
  | Stat {
     }
  deriving (Data, Typeable, Show, Eq)
 
ls :: Options
ls = Ls {
    path = def &= typ "PATH" &= argPos 0
  , long = False &= typ "FLAG"
  } &= details  [ "Examples:"
                , "ls -l /foo/bar"
                ]

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
 
create :: Options
create = Create { 
    path = def &= typ "PATH" &= argPos 0
  , content = def &= typ "CONTENT" &= argPos 1
  } &= details  [ "Examples:"
                , "create /foo/bar baz"
                ]
 
touch :: Options
touch = Touch { 
    path = def &= typ "PATH" &= argPos 0
  } &= details  [ "Examples:"
                , "touch /foo/bar"
                ]
 
rm :: Options
rm = Rm {
    recursive = False &= typ "FLAG"
  } &= details  [ "Examples:"
                , "rm /foo/bar"
                ]

stat :: Options
stat = Stat {
  } &= details  [ "Examples:"
                , "stat"
                ]

commandModes :: Mode (CmdArgs Options)
commandModes = cmdArgsMode $ modes [ls, get, set, create, touch, rm, stat]
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
  Z.setDebugLevel Z.ZLogError
  zsVar <- newTVarIO Z.ConnectingState
  Z.withZookeeper "localhost:2181" 1000 (Just $ watcher zsVar) Nothing $ \z -> do
    aclsVar <- newTVarIO Z.OpenAclUnsafe
    let context = Shell z aclsVar
    if null args then runInputT defaultSettings (shell context zsVar) else do
      opts <- cmdArgsRun commandModes
      optionHandler context opts

shell :: Shell -> TVar Z.State -> InputT IO ()
shell context zsVar = loop
  where
    loop = do         
      zs <- liftIO $ readTVarIO zsVar
      minput <- getInputLine $ "(" ++ prompt zs ++ ")% "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          (liftIO $ execute context $ filter (not .null) $ splitOn " " input) `catch` (\(e :: SomeException) -> return ())
          loop

watcher zsVar z e s _ = case e of
  Z.SessionEvent -> atomically $ writeTVar zsVar s
  _ -> return ()

execute :: Shell -> [String] -> IO ()
execute context args = do
  opts <- withArgs (if null args then ["--help"] else args) $ cmdArgsRun commandModes
  optionHandler context opts

optionHandler :: Shell -> Options -> IO ()
optionHandler c opts@Get {}  = do
    exec c opts
optionHandler c opts@Set {}  = do
    exec c opts
optionHandler c opts = do
    exec c opts
 
exec :: Shell -> Options -> IO ()

exec Shell { sZookeeper = z } opts@Ls {} = do
  let headf = "%8s %8s %8s %s"
      rawf  = "%8d %8d %8d %s"
  e <- Z.getChildren z (path opts) Nothing
  case e of
    Right nodes -> case long opts of
      True -> do
        putStrLn $ printf headf "version" "size" "children" "name"
        forM_ nodes $ \n -> do
          e' <- Z.exists z (concatPath (path opts) n) Nothing
          case e' of
            Right stat -> do -- TODO
              putStrLn $ printf rawf (Z.statVersion stat) (Z.statDataLength stat) (Z.statNumChildren stat) n
            Left zkerr -> hPutStrLn stderr $ show zkerr
      False -> print nodes
    Left zkerr -> hPutStrLn stderr $ show zkerr

exec Shell { sZookeeper = z } opts@Get {} = do
  e <- Z.get z (path opts) Nothing
  case e of
    Right (mData, stat) -> print mData
    Left Z.NoNodeError -> return ()
    Left zkerr -> hPutStrLn stderr $ show zkerr

exec Shell { sZookeeper = z } opts@Set {} = do
  e <- Z.set z (path opts) (Just $ BS.pack $ content opts) Nothing
  case e of
    Right stat -> return ()
    Left zkerr -> hPutStrLn stderr $ show zkerr

exec Shell { sZookeeper = z, sAclsVar = aclsVar } opts@Create {} = do
  acls <- readTVarIO aclsVar
  e <- Z.create z (path opts) (Just $ BS.pack $ content opts) acls []
  case e of
    Right stat -> return ()
    Left zkerr -> hPutStrLn stderr $ show zkerr

exec Shell { sZookeeper = z, sAclsVar = aclsVar } opts@Touch {} = do
  acls <- readTVarIO aclsVar
  e <- Z.create z (path opts) Nothing acls []
  case e of
    Right stat -> return ()
    Left zkerr -> hPutStrLn stderr $ show zkerr

exec c@Shell { sZookeeper = z } opts@Rm {} = case recursive opts of
  True -> do
    e <- Z.getChildren z (path opts) Nothing
    case e of
      Right nodes -> do
        forM_ nodes $ \n -> do
          exec c opts { path = concatPath (path opts) n }
        e' <- Z.delete z (path opts) Nothing
        case e' of
          Right stat -> return ()
          Left zkerr -> hPutStrLn stderr $ show zkerr
      Left zkerr -> hPutStrLn stderr $ show zkerr ++ " (" ++ path opts ++ ")"
  False -> do
    e <- Z.delete z (path opts) Nothing
    case e of
      Right stat -> return ()
      Left zkerr -> hPutStrLn stderr $ show zkerr

exec c opts@Stat {} = do
  putStrLn $ "stat"

concatPath :: String -> String -> String
concatPath p n = ((if p == "/" then "/" else p ++ "/") ++ n)

prompt :: Z.State -> String
prompt state = case state of
  Z.ExpiredSessionState -> "ExpiredSession"
  Z.AuthFailedState -> "AuthFailed"
  Z.ConnectingState -> "Connecting"
  Z.AssociatingState -> "Associating"
  Z.ConnectedState -> "Connected"
  Z.UnknownState x -> "Unknown " ++ show x
