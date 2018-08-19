{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Login (loginPrompt) where

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

data Name = NameField
          | AgeField
          | BikeField
          | HandedField
          | PasswordField
          | LeftHandField
          | RightHandField
          | AmbiField
          | AddressField
          deriving (Eq, Ord, Show)

data Handedness = LeftHanded | RightHanded | Ambidextrous
                deriving (Show, Eq)

data UserInfo =
    UserInfo { _name      :: T.Text
             , _age       :: Int
             , _address   :: T.Text
             , _ridesBike :: Bool
             , _handed    :: Handedness
             , _password  :: T.Text
             }
             deriving (Show)

makeLenses ''UserInfo

-- This form is covered in the Brick User Guide; see the "Input Forms"
-- section.
mkForm :: UserInfo -> Form UserInfo e Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Username" @@=
                   editTextField name NameField (Just 1)
               , label "Password" @@=
                   editPasswordField password PasswordField
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
        body = str $ "- Username\n" <>
                     "- Password\n"

app :: App (Form UserInfo e Name) e Name
app =
    App { appDraw = draw
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (V.EvResize {})     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt s
                -- Enter quits only when we aren't in the multi-line editor.
                VtyEvent (V.EvKey V.KEnter [])
                    | focusGetCurrent (formFocus s) /= Just AddressField ->
                      halt s
                _ -> do
                       s' <- handleFormEvent ev s
                    -- Example of external validation:
                    -- Require age field to contain a value that is at least 18.
                       continue
                         (setFieldValid ((formState s')^.age >= 18) AgeField s')
        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap}

loginPrompt :: IO (String, String)
loginPrompt = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        initialUserInfo = UserInfo { _name = ""
                                   , _address = ""
                                   , _age = 0
                                   , _handed = RightHanded
                                   , _ridesBike = False
                                   , _password = ""
                                   }
        f = setFieldValid False AgeField $
            mkForm initialUserInfo
    f' <- customMain buildVty Nothing app f
    return (T.unpack $ _name $ formState f',
            T.unpack $ _password $ formState f')
