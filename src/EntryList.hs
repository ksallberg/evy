{-# LANGUAGE OverloadedStrings #-}

module EntryList where

import           Table
import qualified Graphics.Vty as Vty

import Brick
import Control.Lens ((&), (^.), (.~), makeLenses)
import Data.Maybe (fromMaybe)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Brick.Types as T
import qualified Data.Vector as Vec

import Types

import Data.Text

desc :: String
desc = "Press a to add. Press q to get quote. Press Esc to exit."

type AppState = ([Entry], Int)

entryListPrompt :: [Entry] -> IO (Maybe String)
entryListPrompt i = do
  (_, retValue) <- defaultMain theApp (i, 0)
  case retValue of
    0 ->
      return Nothing
    1 ->
      return $ Just "add"
    2 ->
      return $ Just "qte"

appEvent :: AppState -> BrickEvent Int e -> EventM Int (Next AppState)
appEvent = \s ev ->
            case ev of
                VtyEvent (Vty.EvResize {})     -> continue s
                VtyEvent (Vty.EvKey Vty.KEsc [])   -> halt s
                VtyEvent (Vty.EvKey Vty.KEnter []) -> halt s
                VtyEvent (Vty.EvKey (Vty.KChar 'a') []) ->
                  halt (fst s, 1)
                VtyEvent (Vty.EvKey (Vty.KChar 'q') []) ->
                  halt ([], 2)
                _ -> halt s

theApp :: App AppState e Int
theApp =
    App { appDraw = ui
          , appChooseCursor = showFirstCursor
          , appHandleEvent = appEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

theMap :: A.AttrMap
theMap = A.attrMap Vty.defAttr
    [
    ]

ui :: AppState -> [Widget Int]
ui (en, _) = [box]
  where
    label = str $ "List portfolio contents"
    box = vBox [(B.borderWithLabel label $
                 hLimit 225 $
                 vLimit 15 $
                 renderGainsTable 1 en),
                C.hCenter $ str desc]

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
    , alignRight  "0"
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
