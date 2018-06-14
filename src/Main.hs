module Main where

import Control.Exception
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
  q <- Logger.new Logger.defSettings
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
  let p = defQueryParams One ()
  res <- runClient (th st) (query q p)
  return $ res /= []

createUser :: String -> String -> String -> IO ()
createUser username email password = do
  putStrLn "hej"

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
        _ ->
          putStrLn $ "user input: " ++ choice
    Just username -> do
      putStrLn "welcome!"
      putStrLn appMenu

login :: EState -> IO EState
login st = do
  putStrLn "enter username"
  username <- getLine
  putStrLn "enter password"
  password <- getPassword
  loginRes <- userAndPasswordExists st username password
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
  putStrLn "enter password"
  password <- getPassword
  putStrLn "enter password again"
  password2 <- getPassword
  putStrLn "enter email"
  email <- getLine
  case password == password2 of
    True -> do
      putStrLn "you are registered and can now login"
      loop st
    False -> do
      putStrLn "passwords not matching"
      loop st

--------------- helpers

loginMenu :: String
loginMenu = "1. Login\n2. Register"

appMenu :: String
appMenu = "1. Ls portfolio"

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
