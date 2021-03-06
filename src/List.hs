{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module List where

import Lens.Micro ((^.))
import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Maybe (fromMaybe, fromJust)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , withAttr
  )
import Brick.Util (fg, on)

desc :: String
desc = "Press a to add. Press q to get quote. Press Esc to exit."

drawUI :: (Show a) => String -> L.List () a -> [Widget ()]
drawUI strx l = [ui]
    where
        label = str strx <+> cur <+> str " of " <+> total
        cur = case l^.(L.listSelectedL) of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.(L.listElementsL)
        box = B.borderWithLabel label $
              hLimit 225 $
              vLimit 15 $
              L.renderList (listDrawElement strx) True l
        ui = C.vCenter $ vBox [ C.hCenter box
                              , str " "
                              , C.hCenter $ str desc
                              ]

appEvent :: L.List () String ->
            T.BrickEvent () e ->
            T.EventM () (T.Next (L.List () String))
appEvent l (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.halt (L.list () (Vec.fromList []) 0)
        V.EvKey V.KEnter [] -> M.halt l
        V.EvKey (V.KChar 'a') [] -> M.halt (L.list () (Vec.fromList ["add"]) 1)
        V.EvKey (V.KChar 'q') [] -> M.halt (L.list () (Vec.fromList ["qte"]) 1)
        ev -> M.continue =<< L.handleListEvent ev l
    where
      cur = case l^.(L.listSelectedL) of
                Nothing -> "-"
                Just i -> (show (i + 1))
appEvent l _ = M.continue l

listDrawElement :: (Show a) => String -> Bool -> a -> Widget ()
listDrawElement strx sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ str strx <+> (selStr $ show a)

initialState :: [String] -> L.List () String
initialState i = L.list () (Vec.fromList i) 1

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.black `on` V.white)
    , (L.listSelectedAttr,    V.black `on` V.red)
    , (customAttr,            fg V.black)
    ]

theApp :: String -> M.App (L.List () String) e ()
theApp str =
    M.App { M.appDraw = (drawUI str)
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

listPrompt :: String -> [String] -> IO (Maybe String)
listPrompt descr i = do
  l <- M.defaultMain (theApp descr) (initialState i)
  case Vec.toList (L.listElements l) of
    []      -> return Nothing
    ["add"] -> return $ Just "add"
    ls -> return $ Just (ls !! (fromJust (L.listSelected l)))
