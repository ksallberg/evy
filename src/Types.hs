module Types where

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple

import Data.Text
import Data.Time.Clock (UTCTime)

data EState = EState {
  th   :: Connection,
  user :: Maybe String
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
                             pname :: Text,
                             owner :: Text
                           } deriving Show

instance FromRow Portfolio where
  fromRow = Portfolio <$> field <*> field <*> field

instance ToRow Portfolio where
  toRow p = [toField (pname p),
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
