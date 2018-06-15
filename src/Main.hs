module Main where

import Control.Exception
import Data.Hash.MD5 (md5s, Str(..))
import Data.Functor.Identity
import Data.Maybe
import Data.String (fromString)
import Data.Text (pack, Text)
import Database.CQL.IO as Client
import Database.CQL.Protocol
import System.IO

import qualified System.Logger as Logger

data EState = EState {
  th :: ClientState,
  user :: Maybe String
}

type X = (Text, Text, Text)

main :: IO ()
main = do
  q <- Logger.new (Logger.setLogLevel Logger.Fatal Logger.defSettings)
  c <- Client.init q defSettings
  loop EState{user = Nothing, th = c}

userExists :: EState -> String -> IO Bool
userExists st usr = do
  let q = fromString $ "SELECT * FROM evy.users WHERE username='" ++ usr ++ "'"
          :: QueryString Client.R () (Text, Text, Text)
  let p = defQueryParams One ()
  res <- runClient (th st) (query q p)
  return $ res /= []

userAndPasswordExists :: EState -> String -> String -> IO Bool
userAndPasswordExists st usr pwd = do
  let q = fromString ("SELECT * FROM evy.users WHERE username='" ++ usr ++ "'"
                      ++ " AND encrypted_password='" ++ pwd ++ "'")
          :: QueryString Client.R () (Text, Text, Text)
      p = defQueryParams One ()
  res <- runClient (th st) (query q p)
  return $ res /= []

createUser :: EState -> String -> String -> String -> IO ()
createUser st username email password = do
  let q = fromString (createUserCQL username password email)
          :: QueryString Client.W () ()
      p = defQueryParams Quorum ()
  runClient (th st) (write q p)

createUserCQL :: String -> String -> String -> String
createUserCQL user pass email =
  "INSERT INTO evy.users (username, encrypted_password, " ++
  "email) VALUES ('" ++ user ++ "', '" ++ pass ++
  "', '" ++ email ++ "')"

loop :: EState -> IO ()
loop st = do
  case (user st) of
    Nothing -> do
      putStrLn loginMenu
      choice <- getLine
      case choice of
        "1" -> do
          newSt <- login st
          loop newSt
        "2" ->
          register st
        "q" -> do
          putStrLn "closing th"
          shutdown (th st)
          putStrLn "good bye"
        _ -> do
          putStrLn "invalid choice"
          loop st
    Just username -> do
      putStrLn $ "welcome " ++ username ++ "!"
      putStrLn appMenu
      choice <- getLine
      case choice of
        "1" -> do
          putStrLn "showing portfolio"
          loop st
        "q" -> do
          putStrLn "logging out"
          loop st{user = Nothing}
        _ -> do
          putStrLn "invalid choice"
          loop st

login :: EState -> IO EState
login st = do
  putStrLn "enter username"
  username <- getLine
  putStrLn "enter password"
  password <- getPassword
  loginRes <- userAndPasswordExists st username (md5s (Str password))
  case loginRes of
    True ->
      return st{user = Just username}
    False -> do
      putStrLn "login failed!"
      return st

register :: EState -> IO ()
register st = do
  putStrLn "enter username"
  username <- getLine
  exists <- userExists st username
  case exists of
    True -> do
      putStrLn "already registered"
      loop st
    False -> do
      putStrLn "enter password"
      password <- getPassword
      putStrLn "enter password again"
      password2 <- getPassword
      putStrLn "enter email"
      email <- getLine
      case password == password2 of
        True -> do
          createUser st username email (md5s (Str password))
          loop st{user = Just username}
        False -> do
          putStrLn "passwords not matching"
          loop st

--------------- helpers

loginMenu :: String
loginMenu = "1. Login\n2. Register\nq quit"

appMenu :: String
appMenu = "1. Ls portfolio\nq quit"

-- https://stackoverflow.com/questions/4064378/ \
-- prompting-for-a-password-in-haskell-command-line-application
getPassword :: IO String
getPassword = do
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
