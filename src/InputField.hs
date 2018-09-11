{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module InputField (inputPrompt) where

import qualified Data.Text as T
import Lens.Micro ((^.))
import Lens.Micro.TH
import Data.Monoid ((<>))

import qualified Graphics.Vty as V
import Brick
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , setFieldValid
  , renderForm
  , handleFormEvent
  , invalidFields
  , allFieldsValid
  , focusedFormInputAttr
  , invalidFormInputAttr
  , checkboxField
  , radioField
  , editShowableField
  , editTextField
  , editPasswordField
  , (@@=)
  )

import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )

import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

data Name = InputField
          deriving (Eq, Ord, Show)

data Handedness = LeftHanded | RightHanded | Ambidextrous
                deriving (Show, Eq)

data UserInfo =
    UserInfo { _input :: T.Text,
               _desc :: T.Text
             }
             deriving (Show)

makeLenses ''UserInfo

-- This form is covered in the Brick User Guide; see the "Input Forms"
-- section.
mkForm :: UserInfo -> Form UserInfo e Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "" @@=
                   editTextField input InputField (Just 1)
               ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

draw :: Form UserInfo e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ (T.unpack $ _desc $ formState f)

app :: App (Form UserInfo e Name) e Name
app =
    App { appDraw = draw
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (V.EvResize {}) -> continue s
                VtyEvent (V.EvKey V.KEnter []) -> halt s
                _ -> do
                  s' <- handleFormEvent ev s
                  continue s'
        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap}

inputPrompt :: String -> IO String
inputPrompt descr = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        initialUserInfo = UserInfo { _input = "", _desc = T.pack descr }
        f = mkForm initialUserInfo
    f' <- customMain buildVty Nothing app f
    return (T.unpack $ _input $ formState f')
