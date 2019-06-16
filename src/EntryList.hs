{-# LANGUAGE OverloadedStrings #-}

module EntryList where

import           Table
import qualified Graphics.Vty as Vty

import Brick
import Control.Lens ((&), (^.), (.~), makeLenses)
import Data.Maybe (fromMaybe)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

import Types

import Data.Text

entryListPrompt :: [Entry] -> IO (Maybe String)
entryListPrompt i = do
  _ <- simpleMain (ui i)
  return (Just "hej")

ui :: [Entry] -> Widget Int
ui en = renderGainsTable 1 en

renderGainsTable :: Int -> [Entry] -> Widget Int
renderGainsTable n ls =
    table TableConfig
        { columns =
            tableColumns "2"
        , footerRows =
            [totalsRow 2]
        , name =
            n
        , showRowDividers =
            True
        } ls

totalsRow :: Int -> [Widget n]
totalsRow decimalPlaces =
    [ str " "
    , str " "
    , str " "
    , alignRight "Totals:"
    , alignRight  "h7ej"
    , alignRight "h6ej"
    , alignRight "h1aj"
    , alignRight "h3oj"
    , alignRight "h5aj"
    ]
    where
        alignRight :: String -> Widget n
        alignRight = padLeft Max . str

tableColumns :: String -> [Column Entry]
tableColumns decimalPlaces =
    [ column
        { headerName = "Symbol"
        , dataSelector = unpack . symbol
        }
    , column
        { headerName = "Units"
        , dataSelector = show . units
        }
    , column
        { headerName = "Price"
        , dataSelector = show . price
        }
    , column
        { headerName = "Type"
        , dataSelector = unpack . etype
        }
    , column
        { headerName = "Timestamp"
        , dataSelector = show . ts
        }
    ]
    where
        column =
            Column
                { headerName = ""
                , headerAlign = Alignment VMiddle HRight
                , dataAlign = Alignment VMiddle HRight
                , columnWeight = 10
                , dataSelector = const ""
                }
