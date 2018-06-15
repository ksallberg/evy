module Main where

import Control.Exception
import Control.Monad (forM_)
import Data.Hash.MD5 (md5s, Str(..))
import Data.Functor.Identity
import Data.List (intersperse)
import Data.Maybe
import Data.String (fromString)
import Data.Text (unpack, pack, Text)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Database.CQL.IO as Client
import Database.CQL.Protocol
import System.IO

import qualified System.Logger as Logger

data EState = EState {
  th :: ClientState,
  user :: Maybe String
}

type UserR = QueryString Client.R () (Text, Text, Text)
type UserW = QueryString Client.W () ()

type PortfR = QueryString Client.R () (Identity Text)
type PortfW = QueryString Client.W () ()

main :: IO ()
main = do
  q <- Logger.new (Logger.setLogLevel Logger.Fatal Logger.defSettings)
  c <- Client.init q defSettings
  loop EState{user = Nothing, th = c}

userExists :: EState -> String -> IO Bool
userExists st usr = do
  let q = fromString $ ("SELECT * FROM evy.users WHERE username='" ++
                        usr ++ "'") :: UserR
  let p = defQueryParams One ()
  res <- runClient (th st) (query q p)
  return $ res /= []

userAndPasswordExists :: EState -> String -> String -> IO Bool
userAndPasswordExists st usr pwd = do
  let q = fromString ("SELECT * FROM evy.users WHERE username='" ++ usr ++ "'"
                      ++ " AND encrypted_password='" ++ pwd ++ "'") :: UserR
      p = defQueryParams One ()
  res <- runClient (th st) (query q p)
  return $ res /= []

getPortfolios :: EState -> IO [String]
getPortfolios st = do
  let q = fromString ("SELECT name FROM evy.portfolios WHERE owner='" ++
                      (fromJust $ user st) ++ "'") :: PortfR
      p = defQueryParams One ()
  res <- runClient (th st) (query q p)
  return $ map (Data.Text.unpack . (\(Identity x) -> x)) res

createPortfolio :: EState -> String -> IO ()
createPortfolio st name = do
  randUUID <- nextRandom
  let q = fromString (createPortfCQL (fromJust $ user st)
                      name (toString randUUID)) :: PortfW
      p = defQueryParams Quorum ()
  runClient (th st) (write q p)

createUser :: EState -> String -> String -> String -> IO ()
createUser st username email password = do
  let q = fromString (createUserCQL username password email) :: UserW
      p = defQueryParams Quorum ()
  runClient (th st) (write q p)

createUserCQL :: String -> String -> String -> String
createUserCQL user pass email =
  "INSERT INTO evy.users (username, encrypted_password, " ++
  "email) VALUES ('" ++ user ++ "', '" ++ pass ++
  "', '" ++ email ++ "')"

createPortfCQL :: String -> String -> String -> String
createPortfCQL user portfName uuid =
  "INSERT INTO evy.portfolios (name, owner, id) VALUES" ++
  "('" ++ portfName ++ "', '" ++ user ++ "', " ++ uuid ++ ")"

loop :: EState -> IO ()
loop st = do
  case (user st) of
    Nothing -> do
      prompt loginMenu
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
      prompt appMenu
      choice <- getLine
      case choice of
        "1" -> do -- list portfolios
          portfolios <- getPortfolios st
          forM_ portfolios putStrLn
          loop st
        "2" -> do -- Ls specific portfolio
          prompt ""
          putStrLn "hej1"
          loop st
        "3" -> do
          prompt "enter portfolio name"
          portfName <- getLine
          createPortfolio st portfName
          loop st
        "4" -> do
          putStrLn "hej3"
          loop st
        "q" -> do
          putStrLn "logging out"
          loop st{user = Nothing}
        _ -> do
          putStrLn "invalid choice"
          loop st

login :: EState -> IO EState
login st = do
  prompt "enter username"
  username <- getLine
  prompt "enter password"
  password <- getPassword
  loginRes <- userAndPasswordExists st username (md5s (Str password))
  case loginRes of
    True -> do
      putStrLn $ "welcome " ++ username ++ "!"
      return st{user = Just username}
    False -> do
      putStrLn "login failed!"
      return st

register :: EState -> IO ()
register st = do
  prompt "enter username"
  username <- getLine
  exists <- userExists st username
  case exists of
    True -> do
      putStrLn "already registered"
      loop st
    False -> do
      prompt "enter password"
      password <- getPassword
      prompt "enter password again"
      password2 <- getPassword
      prompt "enter email"
      email <- getLine
      case password == password2 of
        True -> do
          createUser st username email (md5s (Str password))
          putStrLn $ "welcome " ++ username ++ "!"
          loop st{user = Just username}
        False -> do
          putStrLn "passwords not matching"
          loop st

--------------- helpers

loginMenu :: String
loginMenu = let ls = [ "___________"
                     , "1. Login"
                     , "2. Register"
                     , "q. quit"
                     , "___________"
                     ]
            in concat $ intersperse "\n" ls

appMenu :: String
appMenu = let ls = [ "_______________"
                   , "1. List portfolios"
                   , "2. Ls specific portfolio"
                   , "3. Create portfolio"
                   , "4. Add stock to portfolio"
                   , "q. quit"
                   , "_______________"
                   ]
          in concat $ intersperse "\n" ls

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

prompt :: String -> IO ()
prompt msg = do
  putStrLn msg
  putChar '>'
  hFlush stdout
