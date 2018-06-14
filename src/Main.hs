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
  user :: Maybe String
} deriving (Show)

type X = (Text, Text, Text)

main :: IO ()
main = do
  x <- userExists "bob"
  y <- userExists "ciri"
  putStrLn $ "x: " ++ show x ++ ", y: " ++ show y
-- main = loop EState{user = Nothing}

userExists :: String -> IO Bool
userExists usr = do
  q <- Logger.new Logger.defSettings
  c <- Client.init q defSettings
  let q = fromString $ "SELECT * from evy.users where username='" ++ usr ++ "'"
          :: QueryString Client.R () (Text, Text, Blob)
  let p = defQueryParams One ()
  res <- runClient c (query q p)
  shutdown c
  return $ res /= []

loop :: EState -> IO ()
loop st = do
  case (user st) of
    Nothing -> do
      putStrLn loginMenu
      choice <- getLine
      case choice of
        "1" ->
          login st
        "2" ->
          register st
        _ ->
          putStrLn $ "user input: " ++ choice
    Just username -> do
      putStrLn appMenu
      loop st

login :: EState -> IO ()
login st = do
  putStrLn "enter username"
  username <- getLine
  putStrLn "enter password"
  password <- getPassword
  putStrLn $ "ok! " ++ username ++ ", " ++ password

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
