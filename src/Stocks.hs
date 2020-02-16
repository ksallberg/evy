{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Stocks (getPrice) where

import Control.Exception
import Data.Aeson hiding (Result)
import Data.Maybe
import GHC.Generics
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L8

data Wrapper = Wrapper {
  _optionChain :: InnerWrapper
} deriving (Generic, Show, Eq)

data InnerWrapper = InnerWrapper {
  _result :: [Result],
  _error :: Maybe String
} deriving (Generic, Show, Eq)

data Result = Result {
  _underlyingSymbol :: Maybe String,
  _expirationDates :: [Int],
  _strikes :: [Double],
  _hasMiniOptions :: Bool,
  _quote :: Quote,
  _options :: [Option]
} deriving (Generic, Show, Eq)

data Quote = Quote {
  _language :: String,
  _region :: String,
  _quoteType :: String,
  _triggerable :: Bool,
  _quoteSourceName :: String,
  _currency :: String,
  _financialCurrency :: String,
  _regularMarketOpen :: Double,
  _regularMarketTime :: Double,
  _regularMarketPrice :: Double,
  _regularMarketDayHigh :: Double,
  _regularMarketDayRange :: String,
  _regularMarketDayLow :: Double,
  _regularMarketVolume :: Double,
  _regularMarketPreviousClose :: Double,
  _bid :: Double,
  _ask :: Double,
  _bidSize :: Int,
  _askSize :: Int,
  _exchange :: String,
  _market :: String,
  _messageBoardId :: String,
  _fullExchangeName :: String,
  _shortName :: String,
  _longName :: String,
  _averageDailyVolume3Month :: Int,
  _averageDailyVolume10Day :: Int,
  _fiftyTwoWeekLowChange :: Double,
  _fiftyTwoWeekLowChangePercent :: Double,
  _fiftyTwoWeekRange :: String,
  _fiftyTwoWeekHighChange :: Double,
  _fiftyTwoWeekHighChangePercent :: Double,
  _fiftyTwoWeekLow :: Double,
  _fiftyTwoWeekHigh :: Double,
  _trailingPE :: Double,
  _marketState :: String,
  _epsTrailingTwelveMonths :: Double,
  _epsForward :: Double,
  _sharesOutstanding :: Int,
  _bookValue :: Double,
  _fiftyDayAverage :: Double,
  _fiftyDayAverageChange :: Double,
  _fiftyDayAverageChangePercent :: Double,
  _twoHundredDayAverage :: Double,
  _twoHundredDayAverageChange :: Double,
  _twoHundredDayAverageChangePercent :: Double,
  _marketCap :: Int,
  _forwardPE :: Double,
  _priceToBook :: Double,
  _sourceInterval :: Int,
  _exchangeDataDelayedBy :: Int,
  _exchangeTimezoneName :: String,
  _exchangeTimezoneShortName :: String,
  _gmtOffSetMilliseconds :: Int,
  _esgPopulated :: Bool,
  _tradeable :: Bool,
  _firstTradeDateMilliseconds :: Int,
  _priceHint :: Int,
  _postMarketChangePercent :: Double,
  _postMarketTime :: Int,
  _postMarketPrice :: Double,
  _postMarketChange :: Double,
  _regularMarketChange :: Double,
  _regularMarketChangePercent :: Double,
  _symbol :: String
} deriving (Generic, Show, Eq)

data Option = Option {
  _expirationDate :: Int,
  _hasMiniOptionsx :: Bool,
  _calls :: [Call],
  _puts :: [Call]
} deriving (Generic, Show, Eq)

data Call = Call {
  _contractSymbol :: String,
  _strike :: Double,
  _currencyx :: String,
  _lastPrice :: Double,
  _change :: Double,
  _percentChange :: Double,
  _volume :: Maybe Int,
  _openInterest :: Maybe Int,
  _bidx :: Maybe Double,
  _askx :: Double,
  _contractSize :: String,
  _expiration :: Int,
  _lastTradeDate :: Int,
  _impliedVolatility :: Double,
  _inTheMoney :: Bool
} deriving (Generic, Show, Eq)

customOption =
  defaultOptions {
    fieldLabelModifier = let f "_hasMiniOptionsx" = "hasMiniOptions"
                             f other = (drop 1 other)
                         in f
    }

customCall =
  defaultOptions {
    fieldLabelModifier = let f "_currencyx" = "currency"
                             f "_bidx" = "bid"
                             f "_askx" = "ask"
                             f other = (drop 1 other)
                         in f
    }

instance FromJSON Wrapper where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}
instance FromJSON InnerWrapper where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}
instance FromJSON Result where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}
instance FromJSON Quote where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}
instance FromJSON Option where
  parseJSON = genericParseJSON customOption
instance FromJSON Call where
  parseJSON = genericParseJSON customCall

getPrice :: (String, String) -> IO (Maybe Double)
getPrice ticker = do
  t <- getTicker ticker
  case t of
    Nothing ->
      return Nothing
    (Just wrapper) -> do
      let q = _quote $ head ((_result . _optionChain) wrapper)
      return (Just (_regularMarketPrice q))

getTicker :: (String, String) -> IO (Maybe Wrapper)
getTicker (_token, ticker) = do
  x <- getNonJSONData ticker
  case x of
    Right bytestr -> do
      case (eitherDecode bytestr) of
        Right res ->
          return res
        Left err -> do
          putStrLn $ "error when parsing: " ++ err
          return Nothing
    Left _ -> return Nothing

getNonJSONData :: String -> IO (Either SomeException L8.ByteString)
getNonJSONData ticker = try $ simpleHttp query
  where query = "https://query1.finance.yahoo.com/v7/finance/options/" ++ ticker
