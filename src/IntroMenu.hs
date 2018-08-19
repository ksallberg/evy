module IntroMenu (introMenuPrompt) where

import Data.Maybe (fromJust)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import Brick.Types
  ( Widget
  , BrickEvent(..)
  )
import Brick.Widgets.Core
  ( padAll
  , str
  )
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Util (on, bg)
import qualified Brick.Types as T

drawUI :: D.Dialog String -> [Widget ()]
drawUI d = [ui]
    where
        ui = D.renderDialog d $ C.hCenter $ padAll 1 $ str "select:"

appEvent :: D.Dialog String ->
            BrickEvent () e ->
            T.EventM () (T.Next (D.Dialog String))
appEvent d (VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt d
        V.EvKey V.KEnter [] -> M.halt d
        _ -> M.continue =<< D.handleDialogEvent ev d
appEvent d _ = M.continue d

initialState :: D.Dialog String
initialState = D.dialog (Just "Login menu") (Just (0, choices)) 50
    where
        choices = [ ("Login", "Login")
                  , ("Register", "Register")
                  , ("Quit", "Quit")
                  ]

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr, V.white `on` V.black)
    , (D.buttonAttr, V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.yellow)
    ]

theApp :: M.App (D.Dialog String) e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

introMenuPrompt :: IO String
introMenuPrompt = do
    d <- M.defaultMain theApp initialState
    return $ fromJust (D.dialogSelection d)
