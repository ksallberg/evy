{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad (forM_)
import Data.Hash.MD5 (md5s, Str(..))
import Data.Functor.Identity
import Data.Int (Int32(..), Int64(..))
import Data.List (intersperse)
import Data.Maybe
import Data.String (fromString)
import Data.Text (unpack, pack, Text)
import Data.UUID (toString, UUID(..))
import Data.UUID.V4 (nextRandom)
import Data.Time (getCurrentTime)
import Data.Time.Clock (UTCTime)
import System.Console.ANSI
import System.Environment (getEnv)
import System.IO
import System.IO.Unsafe (unsafePerformIO)

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

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

connectionInfo :: String -> IO Connection
connectionInfo pw = connect defaultConnectInfo {
  connectHost = "10.0.1.253",
  connectDatabase = "evy",
  connectUser = "kristian",
  connectPassword = pw
  }

data EState = EState {
  th   :: Connection,
  user :: Maybe String,
  iexAPIToken :: String
}

data Account = Account { username :: Text,
                         email :: Text,
                         encryptedPassword :: Text
                       } deriving Show

instance FromRow Account where
  fromRow = Account <$> field <*> field <*> field

instance ToRow Account where
  toRow a = [toField (username a),
             toField (email a),
             toField (encryptedPassword a)]

data Portfolio = Portfolio { idnum :: Integer,
                             name :: Text,
                             owner :: Text
                           } deriving Show

instance FromRow Portfolio where
  fromRow = Portfolio <$> field <*> field <*> field

instance ToRow Portfolio where
  toRow p = [toField (name p),
             toField (owner p)]

data Entry = Entry { portid :: Integer,
                     symbol :: Text,
                     etype :: Text,
                     units :: Integer,
                     price :: Double,
                     ts :: UTCTime
                   } deriving Show

instance FromRow Entry where
  fromRow = Entry <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Entry where
  toRow e = [toField (portid e),
             toField (symbol e),
             toField (etype e),
             toField (units e),
             toField (price e),
             toField (ts e)]

main :: IO ()
main = do
  q <- Logger.new (Logger.setLogLevel Logger.Fatal Logger.defSettings)
  pw <- getEnv "EVYPW"
  c <- connectionInfo (pw)
  loop EState{user = Nothing, th = c, iexAPIToken=""}

ui :: Widget ()
ui =
    withBorderStyle unicode $
    borderWithLabel (str "Hello!") $
    (center (str "Left") <+> vBorder <+> center (str "Right"))

allAccounts :: Connection -> IO [Account]
allAccounts c =
  query_ c "SELECT username, email, encrypted_password FROM Account";

userExists :: EState -> String -> IO Bool
userExists st usr = do
  x <- allAccounts (th st)
  return $ elem usr (map (unpack . username) x)

userAndPasswordExists :: EState -> Text -> Text -> IO Bool
userAndPasswordExists st usr pwd = do
  accs <- query_ (th st)
                 "SELECT username, email, encrypted_password FROM Account"
  return $ elem (usr, pwd) [(username acc, encryptedPassword acc) | acc<-accs]

getPortfolios :: EState -> IO [Portfolio]
getPortfolios st = do
  let username = fromJust (user st)
  ports <- query (th st)
                 "SELECT id, name, owner FROM Portfolio WHERE owner=?"
                 [(username)]
  return $ ports

createPortfolio :: EState -> String -> IO Int64
createPortfolio st pname = do
  randUUID <- nextRandom
  execute (th st) q Portfolio{name=Data.Text.pack pname,
                              owner=Data.Text.pack (fromJust $ user st)}
  where q = "INSERT INTO Portfolio (name, owner) VALUES (?, ?)"

lsPortfolio :: EState -> String -> IO (Maybe String)
lsPortfolio st portfolioName = do
  let q = "SELECT portfolio_id, symbol, type, \
           \units, price, ts FROM Entry WHERE portfolio_id=?"
  portfolioID <- portfolioNameToID st portfolioName
  portfolioList <- (query (th st)
                    q
                    [(portfolioID)]) :: IO [Entry]
  let plist2 = [(st, p) | p <- portfolioList]
  listPrompt "_" (map formatStock plist2)

formatStock (state, Entry{symbol=name,
                          ts=date,
                          price=price
                          }) =
  strName ++ (show date) ++ (getDiff state strName price)
  where strName = (Data.Text.unpack name)

getDiff :: EState -> String -> Double -> String
getDiff state ticker oldprice =
  case (unsafePerformIO (Stocks.getPrice (iexAPIToken state, ticker))) of
    Nothing ->
      "?%"
    Just newPrice ->
      " |" ++ show newPrice ++ "| "

lsPortfolios :: EState -> IO (Maybe String)
lsPortfolios st = do
  portfolios <- getPortfolios st
  let names = [Data.Text.unpack (name p) | p <- portfolios]
  listPrompt "Portfolio" names

createUser :: EState -> Account -> IO Int64
createUser st account = execute (th st) q account
  where q = "INSERT INTO Account (username, email, \
             \encrypted_password) VALUES (?, ?, ?)"

createEntry :: EState -> String -> String -> IO ()
createEntry = undefined

-- createEntry :: EState -> String -> String -> IO ()
-- createEntry st portfolioName stockSymbol = do
--   randUUID <- nextRandom
--   portfolioUUID0 <- portfolioNameToID st portfolioName
--   curT <- getCurrentTime
--   case portfolioUUID0 of
--     Just portfolioUUID -> do
--       priceAsk <- Stocks.getPrice (iexAPIToken st, stockSymbol)
--       case priceAsk of
--         Nothing ->
--           putStrLn $ "Error: could not fetch price for " ++ stockSymbol
--         Just (price) -> do
--           let timestamp = "'" ++ (take 19 (show curT)) ++ "-0200'"
--               q = fromString (createEntryCQL (toString randUUID)
--                               portfolioUUID stockSymbol
--                               timestamp (show price)) :: EntryW
--               p = mkQueryParams
--           runClient (th st) (write q p)
--     Nothing ->
--       putStrLn $ "Error: portfolio '" ++ portfolioName ++ "' is not existing"

portfolioNameToID :: EState -> String -> IO Integer
portfolioNameToID st portfolioName = do
  let username = fromJust (user st)
  portId <- (query (th st)
             "SELECT id, name, owner FROM Portfolio WHERE name=? AND owner=?"
             [(portfolioName), (username)]) :: IO [Portfolio]
  let idForPort = (idnum (head portId))
  return idForPort

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
      close (th st)
      return ()

getQuote :: EState -> IO ()
getQuote st = do
  stockName <- inputPrompt "enter stock name"
  pr <- Stocks.getPrice (iexAPIToken st, stockName)
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
  loginRes <- userAndPasswordExists st (pack username)
                                       (pack (md5s (Str password)))
  case loginRes of
    True -> do
      apiToken <- getEnv "IEXAPITOKEN"
      return st{user = Just username, iexAPIToken = apiToken}
    False -> do
      return st

register :: EState -> IO ()
register st = do
  (usernamex, password, password2, emailx) <- registerPrompt
  exists <- userExists st usernamex
  case exists of
    True -> do
      loop st
    False -> do
      case password == password2 of
        True -> do
          let acc = Account{username=pack usernamex,
                            email=pack emailx,
                            encryptedPassword=pack (md5s (Str password))}
          createUser st acc
          loop st{user = Just usernamex}
        False -> do
          loop st
