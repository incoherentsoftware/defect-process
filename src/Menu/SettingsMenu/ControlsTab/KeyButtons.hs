module Menu.SettingsMenu.ControlsTab.KeyButtons
    ( ControlsKeyButton(..)
    , loadControlsKeyButtons
    , updateControlsKeyButton
    , drawControlsKeyButton
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Traversable       (for)
import qualified Data.HashMap.Lazy as HM
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.Text as T
import qualified SDL.Raw

import Configs
import Configs.All.Settings
import Configs.All.Settings.Menu
import FileCache
import Menu.SettingsMenu.ControlsTab.KeyButtons.JSON
import Menu.ZIndex
import Msg
import Util
import Window.Graphics
import Window.Graphics.UiControls.Button
import Window.InputState

settingsMenuPack           = \p -> PackResourceFilePath "data/menu/settings-menu.pack" p
controlsKeyButtonImagePath = settingsMenuPack "controls-key-button.image" :: PackResourceFilePath

waitingForInputText = T.pack "Waiting for input..." :: T.Text
controlsTextColor   = Color 255 255 255 255         :: Color

cancelInputRawDatas = S.fromList
    [ KeyRawData SDL.Raw.SDL_SCANCODE_ESCAPE
    , GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_START
    ]

-- right thumbstick reserved for cursor aiming
noRemapInputRawDatas = S.fromList
    [ GamepadNegAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_RIGHTX
    , GamepadNegAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_RIGHTY
    , GamepadPosAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_RIGHTX
    , GamepadPosAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_RIGHTY
    , GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_GUIDE
    ] :: S.Set InputRawData

data ControlsKeyButton = ControlsKeyButton
    { _type              :: InputType
    , _inputAlias        :: InputAlias
    , _inputRawDatas     :: [InputRawData]
    , _index             :: Int
    , _upIndex           :: Int
    , _downIndex         :: Int
    , _leftIndex         :: Int
    , _rightIndex        :: Int
    , _button            :: Button
    , _inputDisplayText  :: InputDisplayText
    , _isWaitingForInput :: Bool
    }

filterType :: InputType -> [InputRawData] -> [InputRawData]
filterType keyBtnType aliasRawDatas = case keyBtnType of
    MouseKbInputType -> filterInputRawDataMouseKb aliasRawDatas
    GamepadInputType -> filterInputRawDataGamepad aliasRawDatas

lookupInputAliasRawDatas :: InputRead m => InputType -> InputAlias -> m [InputRawData]
lookupInputAliasRawDatas keyBtnType inputAlias = do
    inputAliasRawDataMap <- _inputAliasRawDataMap <$> readInputState
    return $ filterType keyBtnType (HM.lookupDefault [] inputAlias inputAliasRawDataMap)

mkControlsKeyButton
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => InputType
    -> ControlsKeyButtonJSON
    -> m ControlsKeyButton
mkControlsKeyButton keyBtnType keyBtnJSON =
    let
        inputAlias                               = _inputAlias (keyBtnJSON :: ControlsKeyButtonJSON)
        (idx, upIdx, downIdx, leftIdx, rightIdx) = _indices keyBtnJSON
    in do
        inputRawDatas <- lookupInputAliasRawDatas keyBtnType inputAlias

        let txt          = "{" <> prettyShow inputAlias <> "}"
        inputDisplayTxt <- mkInputDisplayText txt Font22 controlsTextColor

        let pos = _pos (keyBtnJSON :: ControlsKeyButtonJSON)
        btn    <- mkImageButton pos controlsKeyButtonImagePath

        return $ ControlsKeyButton
            { _type              = keyBtnType
            , _inputAlias        = inputAlias
            , _inputRawDatas     = inputRawDatas
            , _index             = idx
            , _upIndex           = upIdx
            , _downIndex         = downIdx
            , _leftIndex         = leftIdx
            , _rightIndex        = rightIdx
            , _button            = btn
            , _inputDisplayText  = inputDisplayTxt
            , _isWaitingForInput = False
            }

loadControlsKeyButtons
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => InputType
    -> m (IM.IntMap ControlsKeyButton)
loadControlsKeyButtons keyBtnType = do
    keyBtnJSONs <- readSettingsConfig _menu _settingsControlsTabControlsKeyButtons
    keyBtns     <-
        for keyBtnJSONs $ \keyBtnJSON -> do
            keyBtn <- mkControlsKeyButton keyBtnType keyBtnJSON
            return (_index keyBtn, keyBtn)
    return $ IM.fromList keyBtns

updateControlsKeyButton
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsWrite MenuMsgsPhase m)
    => Button
    -> ControlsKeyButton
    -> m ControlsKeyButton
updateControlsKeyButton btn keyBtn = do
    inputState <- readInputState
    let
        inputAlias        = _inputAlias (keyBtn :: ControlsKeyButton)
        isWaitingForInput = _isWaitingForInput keyBtn
        prevInputRawDatas = _inputRawDatas keyBtn
        keyBtnType        = _type (keyBtn :: ControlsKeyButton)
        isClearKeyPressed = MenuClearKeyAlias `aliasPressed` inputState

    when (_isSelected btn && not isWaitingForInput && isClearKeyPressed) $
        writeMsgs
            [ mkMsg $ ConsoleMsgSetControlsInputAlias inputAlias prevInputRawDatas Nothing
            , mkMsg ConsoleMsgSaveSettings
            , mkMsg $ MenuMsgControlsShowNotification T.empty
            ]

    isWaitingForInput' <- if
        | isWaitingForInput -> case _pressedInputRawData inputState of
            Just inputRawData
                | inputRawData `S.member` cancelInputRawDatas -> do
                    writeMsgs [mkMsg $ MenuMsgControlsShowNotification T.empty]
                    return False

                | filterType keyBtnType [inputRawData] == [] -> do
                    writeMsgs
                        [ mkMsg MenuMsgControlsToggleView
                        , mkMsg $ MenuMsgControlsShowNotification T.empty
                        ]
                    return False

                | inputRawData `S.member` noRemapInputRawDatas -> return True

                | otherwise -> do
                    writeMsgs
                        [ mkMsg $ ConsoleMsgSetControlsInputAlias inputAlias prevInputRawDatas (Just inputRawData)
                        , mkMsg ConsoleMsgSaveSettings
                        , mkMsg $ MenuMsgControlsShowNotification T.empty
                        ]
                    return False

            _ -> do
                writeMsgs [mkMsg $ MenuMsgControlsShowNotification waitingForInputText]
                return True

        | otherwise -> do
            when (_isPressed btn) $
                writeMsgs [mkMsg $ MenuMsgControlsShowNotification waitingForInputText]
            return $ _isPressed btn

    inputRawDatas   <- lookupInputAliasRawDatas keyBtnType inputAlias
    inputDisplayTxt <- updateInputDisplayText $ _inputDisplayText keyBtn

    return $ keyBtn
        { _button            = btn
        , _inputRawDatas     = inputRawDatas
        , _inputDisplayText  = inputDisplayTxt
        , _isWaitingForInput = isWaitingForInput'
        }

drawControlsKeyButton :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => ControlsKeyButton -> m ()
drawControlsKeyButton keyBtn = do
    let
        btn        = _button keyBtn
        isSelected = _isSelected btn || _isWaitingForInput keyBtn
    drawButton menuOverZIndex (btn {_isSelected = isSelected})  -- force drawing as selected if waiting for input

    displayTxtOffset <-
        readSettingsConfig (_menu :: SettingsConfig -> MenuConfig) _settingsControlsTabControlsKeyButtonsValueOffset

    let
        drawInputDisplayTxt = case _type (keyBtn :: ControlsKeyButton) of
            MouseKbInputType -> drawInputDisplayTextMouseKbCentered
            GamepadInputType -> drawInputDisplayTextGamepadCentered
        displayTxtPos       = _pos (btn :: Button) `vecAdd` displayTxtOffset
    drawInputDisplayTxt displayTxtPos menuOverZIndex (_inputDisplayText keyBtn)
