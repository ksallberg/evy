{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad (forM_)
import Data.Hash.MD5 (md5s, Str(..))
import Data.Functor.Identity
import Data.Int (Int32(..))
import Data.List (intersperse)
import Data.Maybe
import Data.String (fromString)
import Data.Text (unpack, pack, Text)
import Data.UUID (toString, UUID(..))
import Data.UUID.V4 (nextRandom)
import Database.CQL.IO as Client
import Database.CQL.Protocol
import System.Console.ANSI
import System.IO

import qualified System.Logger as Logger
import qualified Net.Stocks as Stocks
import qualified Net.IEX.PriceTime as PriceTime

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import InputField (inputPrompt)
import Register (registerPrompt)
import Login (loginPrompt)
import IntroMenu (introMenuPrompt)
import List (listPrompt)

data EState = EState {
  th   :: ClientState,
  user :: Maybe String
}

type UserR = QueryString Client.R () (Text, Text, Text)
type UserW = QueryString Client.W () ()

type PortfR = QueryString Client.R () (Identity Text)
type PortfW = QueryString Client.W () ()

type EntryR = QueryString Client.R () (UUID, UUID, Float, Text, Text, Int32)
type EntryW = QueryString Client.W () ()

main :: IO ()
main = do
  q <- Logger.new (Logger.setLogLevel Logger.Fatal Logger.defSettings)
  c <- Client.init q defSettings
  loop EState{user = Nothing, th = c}

ui :: Widget ()
ui =
    withBorderStyle unicode $
    borderWithLabel (str "Hello!") $
    (center (str "Left") <+> vBorder <+> center (str "Right"))

evyUsers :: String
evyUsers = "SELECT username, email, encrypted_password FROM evy.users WHERE"

-- (UUID, UUID, Float, Text, Text, Int32)
-- \(id, portid, price, name, _type, units)
evyEntries :: String
evyEntries = "SELECT id, portfolio_id, price, symbol, type, units" ++
             " FROM evy.entry WHERE portfolio_id="

userExists :: EState -> String -> IO Bool
userExists st usr = do
  let q = fromString $ (evyUsers ++ " username='" ++
                        usr ++ "'") :: UserR
  let p = defQueryParams One ()
  res <- runClient (th st) (query q p)
  return $ res /= []

userAndPasswordExists :: EState -> String -> String -> IO Bool
userAndPasswordExists st usr pwd = do
  let q = fromString (evyUsers ++ " username='" ++ usr ++ "'"
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
      p = mkQueryParams
  runClient (th st) (write q p)

lsPortfolio :: EState -> String -> IO (Maybe String)
lsPortfolio st portfolioName = do
  portfolioUUID0 <- portfolioNameToID st portfolioName
  case portfolioUUID0 of
    Just portfolioUUID -> do
      let cql = evyEntries ++ portfolioUUID
          q = fromString cql :: EntryR
          p = defQueryParams One ()
          formatF2 = \(id, portid, price, name, _type, units) ->
            Data.Text.unpack name
      res <- runClient (th st) (query q p)
      listPrompt "Stock " (map formatF2 res)
    Nothing -> do
      putStrLn $ "Error: portfolio '" ++ portfolioName ++ "' is not existing"
      return Nothing

lsPortfolios :: EState -> IO (Maybe String)
lsPortfolios st = do
  portfolios <- getPortfolios st
  listPrompt "Portfolio" portfolios

createUser :: EState -> String -> String -> String -> IO ()
createUser st username email password = do
  let q = fromString (createUserCQL username password email) :: UserW
      p = mkQueryParams
  runClient (th st) (write q p)

createEntry :: EState -> String -> String -> IO ()
createEntry st portfolioName stockSymbol = do
  randUUID <- nextRandom
  portfolioUUID0 <- portfolioNameToID st portfolioName
  case portfolioUUID0 of
    Just portfolioUUID -> do
      let q = fromString (createEntryCQL (toString randUUID)
                          portfolioUUID stockSymbol) :: EntryW
          p = mkQueryParams
      runClient (th st) (write q p)
    Nothing ->
      putStrLn $ "Error: portfolio '" ++ portfolioName ++ "' is not existing"

portfolioNameToID :: EState -> String -> IO (Maybe String)
portfolioNameToID st portfolioName = do
  let cql = "SELECT id FROM evy.portfolios WHERE name='" ++ portfolioName ++ "'"
      q = fromString cql :: QueryString Client.R () (Identity UUID)
      p = defQueryParams One ()
  unBoxed <- runClient (th st) (query q p)
  case unBoxed of
    [Identity res] ->
      return $ Just (toString res)
    _ ->
      return Nothing

createUserCQL :: String -> String -> String -> String
createUserCQL user pass email =
  "INSERT INTO evy.users (username, encrypted_password, " ++
  "email) VALUES ('" ++ user ++ "', '" ++ pass ++
  "', '" ++ email ++ "')"

createPortfCQL :: String -> String -> String -> String
createPortfCQL user portfName uuid =
  "INSERT INTO evy.portfolios (name, owner, id) VALUES" ++
  "('" ++ portfName ++ "', '" ++ user ++ "', " ++ uuid ++ ")"

createEntryCQL :: String -> String -> String -> String
createEntryCQL id portfolioID stockSymbol =
  "INSERT INTO evy.entry (id, portfolio_id, symbol, type, units, price) VALUES"
  ++ " ("++ id ++", " ++ portfolioID ++ ", '" ++ stockSymbol ++
  "', 'buy', 1, 1.0)"

loop :: EState -> IO ()
loop st = do
  case (user st) of
    Nothing -> preLogin st
    Just username -> displayPortfolio st

preLogin :: EState -> IO ()
preLogin st = do
  choice <- introMenuPrompt
  case choice of
    "Login" -> do
      newSt <- login st
      loop newSt
    "Register" ->
      register st
    "Quit" -> do
      shutdown (th st)
      return ()

getQuote :: EState -> IO ()
getQuote st = do
  stockName <- inputPrompt "enter stock name"
  pr <- Stocks.getPrice stockName
  _ <- inputPrompt $ "price: " ++ show (fromJust pr)
  return ()

displayPortfolio :: EState -> IO ()
displayPortfolio st = do
  choice <- lsPortfolios st
  case choice of
    Nothing -> loop st{user = Nothing}
    Just "qte" -> do
      getQuote st
      displayPortfolio st
    Just "add" -> do
      portfName <- inputPrompt "enter portfolio name"
      createPortfolio st portfName
      displayPortfolio st
    Just chosenPortfolio -> displayPortfolioChosen st chosenPortfolio

-- when we are inside a portfolio
displayPortfolioChosen :: EState -> String -> IO ()
displayPortfolioChosen st name = do
  portfolioAns <- lsPortfolio st name
  case portfolioAns of
    Just "qte" -> do
      getQuote st
      displayPortfolioChosen st name
    Just "add" -> do
      stockSymbol <- inputPrompt "enter stock symbol"
      createEntry st name stockSymbol
      displayPortfolioChosen st name
    _ ->
      displayPortfolio st

login :: EState -> IO EState
login st = do
  (username, password) <- loginPrompt
  loginRes <- userAndPasswordExists st username (md5s (Str password))
  case loginRes of
    True -> do
      return st{user = Just username}
    False -> do
      return st

register :: EState -> IO ()
register st = do
  (username, password, password2, email) <- registerPrompt
  exists <- userExists st username
  case exists of
    True -> do
      loop st
    False -> do
      case password == password2 of
        True -> do
          createUser st username email (md5s (Str password))
          loop st{user = Just username}
        False -> do
          loop st

--------------- helpers

mkQueryParams = defQueryParams One () -- defQueryParams Quorum ()
