module Menu.SettingsMenu.ControlsTab
    ( mkSettingsControlsTab
    , updateSettingsControlsTab
    , drawSettingsControlsTab
    , isControlsTabWaitingForInput
    ) where

import Control.Monad          (when, void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (evalStateT, get, lift, put)
import Data.Foldable          (traverse_)
import Data.Functor           ((<&>))
import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.Text as T

import Audio.Fmod
import Configs
import Configs.All.Settings
import Configs.All.Settings.Menu
import FileCache
import Menu.SettingsMenu.ControlsTab.KeyButtons
import Menu.SettingsMenu.ControlsTab.Types
import Menu.SettingsMenu.Types
import Menu.SettingsMenu.Util
import Menu.SoundIndices
import Menu.ZIndex
import Msg
import Util
import Window.Graphics
import Window.Graphics.UiControls.Button
import Window.InputState

settingsMenuPack             = \p -> PackResourceFilePath "data/menu/settings-menu.pack" p
controlsBtnImgPath           = settingsMenuPack "controls-button.image"               :: PackResourceFilePath
graphicsBtnImgPath           = settingsMenuPack "graphics-button-inactive.image"      :: PackResourceFilePath
audioBtnImgPath              = settingsMenuPack "audio-button-inactive.image"         :: PackResourceFilePath
gameBtnImgPath               = settingsMenuPack "game-button-inactive.image"          :: PackResourceFilePath
creditsBtnImgPath            = settingsMenuPack "credits-button-inactive.image"       :: PackResourceFilePath
backgroundImgPath            = settingsMenuPack "controls-background.image"           :: PackResourceFilePath
cursorLockOnOverlayImgPath   = settingsMenuPack "cursor-lock-on-overlay.image"        :: PackResourceFilePath
showMouseKbButtonImgPath     = settingsMenuPack "controls-show-mouse-kb-button.image" :: PackResourceFilePath
showGamepadButtonImgPath     = settingsMenuPack "controls-show-gamepad-button.image"  :: PackResourceFilePath
restoreDefaultsButtonImgPath = settingsMenuPack "restore-default-controls.image"      :: PackResourceFilePath

notificationTextColor = Color 150 150 150 255 :: Color
notificationTextPos   = Pos2 960.0 493.0      :: Pos2

rebindPromptCenterTextPos@(Pos2 rebindPromptCenterTextX rebindPromptCenterTextY) = Pos2 1336.0 810.0 :: Pos2
rebindPromptSpacerWidth                                                          = 30.0              :: Float

rebindKeyText    = "Rebind Key: {MenuSelectAlias.0}" :: T.Text
clearKeyText     = "Clear Key: {MenuClearKeyAlias}"  :: T.Text
cancelRebindText = "Cancel: {MenuAlias}"             :: T.Text

mkSettingsControlsTab :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m) => m SettingsControlsTab
mkSettingsControlsTab = do
    tabBtns <-
        mkSettingsTabButtons controlsBtnImgPath graphicsBtnImgPath audioBtnImgPath gameBtnImgPath creditsBtnImgPath

    backgroundImg          <- loadPackImage backgroundImgPath
    cursorLockOnOverlayImg <- loadPackImage cursorLockOnOverlayImgPath
    mouseKbKeyButtons      <- loadControlsKeyButtons MouseKbInputType
    gamepadKeyButtons      <- loadControlsKeyButtons GamepadInputType

    menuCfg             <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)
    let showToggleBtnPos = _settingsControlsTabShowToggleButtonPos menuCfg
    showMouseKbBtn      <- mkImageButtonCentered showToggleBtnPos showMouseKbButtonImgPath
    showGamepadBtn      <- mkImageButtonCentered showToggleBtnPos showGamepadButtonImgPath

    let restoreDefaultsBtnPos = _settingsControlsTabRestoreDefaultsButtonPos menuCfg
    restoreDefaultsBtn       <- mkImageButtonCentered restoreDefaultsBtnPos restoreDefaultsButtonImgPath

    notificationDisplayTxt      <- mkDisplayText T.empty Font36 notificationTextColor
    rebindKeyInputDisplayTxt    <- mkInputDisplayText rebindKeyText Font29 whiteColor
    clearKeyInputDisplayTxt     <- mkInputDisplayText clearKeyText Font29 whiteColor
    cancelRebindInputDisplayTxt <- mkInputDisplayText cancelRebindText Font29 whiteColor

    return $ SettingsControlsTab
        { _buttons                      = tabBtns
        , _backgroundImage              = backgroundImg
        , _cursorLockOnOverlayImage     = cursorLockOnOverlayImg
        , _isInitialUpdate              = True
        , _showToggleButtonType         = MouseKbInputType
        , _mouseKbKeyButtonsMap         = mouseKbKeyButtons
        , _gamepadKeyButtonsMap         = gamepadKeyButtons
        , _showMouseKbButton            = showMouseKbBtn
        , _showGamepadButton            = showGamepadBtn
        , _restoreDefaultsButton        = restoreDefaultsBtn
        , _subSelection                 = ControlsNoSubSelection
        , _notificationDisplayText      = notificationDisplayTxt
        , _rebindKeyInputDisplayText    = rebindKeyInputDisplayTxt
        , _clearKeyInputDisplayText     = clearKeyInputDisplayTxt
        , _cancelRebindInputDisplayText = cancelRebindInputDisplayTxt
        }

updateSelections
    :: (ConfigsRead m, InputRead m)
    => SettingsMenuSelection
    -> SettingsControlsTab
    -> m (SettingsMenuSelection, ControlsSubSelection)
updateSelections selection controlsTab = do
    inputState <- readInputState
    let
        upPressed    = MenuUpAlias `aliasPressed` inputState
        downPressed  = MenuDownAlias `aliasPressed` inputState
        leftPressed  = MenuLeftAlias `aliasPressed` inputState
        rightPressed = MenuRightAlias `aliasPressed` inputState

    menuCfg <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)
    let
        topDefaultKeyBtnSubSelect =
            ControlsKeyButtonSubSelection $ _settingsControlsTabControlsKeyButtonsTopDefaultIndex menuCfg
        botDefaultKeyBtnSubSelect =
            ControlsKeyButtonSubSelection $ _settingsControlsTabControlsKeyButtonsBotDefaultIndex menuCfg

        subSelection = _subSelection (controlsTab :: SettingsControlsTab)

    return $ case selection of
        _
            | isControlsTabWaitingForInput controlsTab -> (selection, subSelection)

        SettingsMenuControlsTabSelection
            | upPressed    -> (SettingsMenuCloseSelection, ControlsNoSubSelection)
            | downPressed  -> (SettingsMenuControlsSubSelection, topDefaultKeyBtnSubSelect)
        SettingsMenuGraphicsTabSelection
            | upPressed    -> (SettingsMenuCloseSelection, ControlsNoSubSelection)
            | downPressed  -> (SettingsMenuControlsSubSelection, topDefaultKeyBtnSubSelect)
        SettingsMenuAudioTabSelection
            | upPressed    -> (SettingsMenuCloseSelection, ControlsNoSubSelection)
            | downPressed  -> (SettingsMenuControlsSubSelection, topDefaultKeyBtnSubSelect)
        SettingsMenuGameTabSelection
            | upPressed    -> (SettingsMenuCloseSelection, ControlsNoSubSelection)
            | downPressed  -> (SettingsMenuControlsSubSelection, topDefaultKeyBtnSubSelect)
        SettingsMenuCreditsTabSelection
            | upPressed    -> (SettingsMenuCloseSelection, ControlsNoSubSelection)
            | downPressed  -> (SettingsMenuControlsSubSelection, topDefaultKeyBtnSubSelect)
        SettingsMenuCloseSelection
            | upPressed    -> (SettingsMenuControlsTabSelection, botDefaultKeyBtnSubSelect)
            | downPressed  -> (SettingsMenuControlsTabSelection, ControlsNoSubSelection)
            | leftPressed  -> (SettingsMenuControlsSubSelection, ControlsShowToggleSubSelection)
            | rightPressed -> (SettingsMenuControlsSubSelection, ControlsRestoreDefaultsSubSelection)

        SettingsMenuControlsSubSelection -> case subSelection of
            ControlsShowToggleSubSelection
                | upPressed    -> (SettingsMenuControlsSubSelection, botDefaultKeyBtnSubSelect)
                | downPressed  -> (SettingsMenuControlsTabSelection, ControlsNoSubSelection)
                | leftPressed  -> (SettingsMenuControlsSubSelection, ControlsRestoreDefaultsSubSelection)
                | rightPressed -> (SettingsMenuCloseSelection, ControlsNoSubSelection)
            ControlsRestoreDefaultsSubSelection
                | upPressed    -> (SettingsMenuControlsSubSelection, botDefaultKeyBtnSubSelect)
                | downPressed  -> (SettingsMenuControlsTabSelection, ControlsNoSubSelection)
                | leftPressed  -> (SettingsMenuCloseSelection, ControlsNoSubSelection)
                | rightPressed -> (SettingsMenuControlsSubSelection, ControlsShowToggleSubSelection)

            ControlsKeyButtonSubSelection idx ->
                let
                    keyBtnsMap = case _showToggleButtonType controlsTab of
                        MouseKbInputType -> _gamepadKeyButtonsMap controlsTab
                        GamepadInputType -> _mouseKbKeyButtonsMap controlsTab
                in case keyBtnsMap IM.!? idx of
                    Just keyBtn
                        | upPressed ->
                            let upIdx = _upIndex keyBtn
                            in if
                                | upIdx < 0 -> (SettingsMenuControlsTabSelection, ControlsNoSubSelection)
                                | otherwise ->
                                    (SettingsMenuControlsSubSelection, ControlsKeyButtonSubSelection upIdx)

                        | downPressed ->
                            let downIdx = _downIndex keyBtn
                            in if
                                | downIdx < 0 -> (SettingsMenuCloseSelection, ControlsNoSubSelection)
                                | otherwise   ->
                                    (SettingsMenuControlsSubSelection, ControlsKeyButtonSubSelection downIdx)

                        | leftPressed ->
                            let leftIdx = _leftIndex keyBtn
                            in (SettingsMenuControlsSubSelection, ControlsKeyButtonSubSelection leftIdx)

                        | rightPressed ->
                            let rightIdx = _rightIndex keyBtn
                            in (SettingsMenuControlsSubSelection, ControlsKeyButtonSubSelection rightIdx)

                    _ -> (SettingsMenuControlsSubSelection, ControlsKeyButtonSubSelection idx)

            _ -> (SettingsMenuControlsSubSelection, subSelection)

        _ -> (selection, ControlsNoSubSelection)

updateTabButtons :: SettingsMenuData -> SettingsTabButtons -> (SettingsTabChoice, SettingsTabButtons)
updateTabButtons settingsMenuData tabBtns
    | _isPressed (_graphicsButton tabBtns :: Button) =
        ( GraphicsTabChoice
        , _buttons (_graphicsTab settingsMenuData :: SettingsGraphicsTab)
        )
    | _isPressed (_audioButton tabBtns :: Button)    =
        ( AudioTabChoice
        , _buttons (_audioTab settingsMenuData :: SettingsAudioTab)
        )
    | _isPressed (_gameButton tabBtns :: Button)     =
        ( GameTabChoice
        , _buttons (_gameTab settingsMenuData :: SettingsGameTab)
        )
    | _isPressed (_creditsButton tabBtns :: Button)  =
        ( CreditsTabChoice
        , _buttons (_creditsTab settingsMenuData :: SettingsCreditsTab)
        )
    | otherwise                                      = (ControlsTabChoice, tabBtns)

writeRestoreDefaultsMsgs :: MsgsWrite MenuMsgsPhase m => m ()
writeRestoreDefaultsMsgs = writeMsgs
    [ mkMsg ConsoleMsgRestoreDefaultSettingsControls
    , mkMsg ConsoleMsgSaveSettings
    ]

readInitialShowToggleButtonType :: InputRead m => m InputType
readInitialShowToggleButtonType = (_lastUsedInputType <$> readInputState) <&> \case
    GamepadInputType -> MouseKbInputType
    MouseKbInputType -> GamepadInputType

processToggleViewMessages :: MsgsRead MenuMsgsPhase m => InputType -> m InputType
processToggleViewMessages showToggleBtnType = L.foldl' processMsg showToggleBtnType <$> readMsgs
    where
        processMsg :: InputType -> MenuMsgPayload -> InputType
        processMsg !inputType d = case d of
            MenuMsgControlsToggleView -> case inputType of
                MouseKbInputType -> GamepadInputType
                GamepadInputType -> MouseKbInputType
            _                         -> inputType

processNotificationMessages :: MsgsRead MenuMsgsPhase m => SettingsControlsTab -> m DisplayText
processNotificationMessages controlsTab = L.foldl' processMsg (_notificationDisplayText controlsTab) <$> readMsgs
    where
        processMsg :: DisplayText -> MenuMsgPayload -> DisplayText
        processMsg !displayTxt d = case d of
            MenuMsgControlsShowNotification txt -> updateDisplayText txt displayTxt
            _                                   -> displayTxt

updateSettingsControlsTab
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsReadWrite MenuMsgsPhase m)
    => SettingsMenuSelection
    -> SettingsTabButtons
    -> SettingsMenuData
    -> m (SettingsMenuData -> SettingsMenuData)
updateSettingsControlsTab selection tabBtns settingsMenuData =
    let
        (tabChoice, tabBtns') = updateTabButtons settingsMenuData tabBtns
        controlsTab           = _controlsTab settingsMenuData
        isWaitingForInput     = isControlsTabWaitingForInput controlsTab
    in do
        (selection', subSelection) <- updateSelections selection controlsTab

        flip evalStateT (selection', subSelection) $
            let
                updateButtonAndSelection = \btnSubSelect btn -> do
                    btnStatus <- get <&> \(_, subSelect) -> if
                        | isWaitingForInput         -> ButtonInactiveStatus
                        | subSelect == btnSubSelect -> ButtonSelectedActiveStatus
                        | otherwise                 -> ButtonActiveStatus
                    btn'      <- lift $ updateButton btnStatus btn

                    when (_isSelected (btn' :: Button) || _isPressed (btn' :: Button)) $
                        put (SettingsMenuControlsSubSelection, btnSubSelect)
                    return btn'

                updateKeyButtonsMap = \keyBtnsMap ->
                    let
                        update = \idx keyBtn -> do
                            btn <- updateButtonAndSelection (ControlsKeyButtonSubSelection idx) (_button keyBtn)
                            lift $ updateControlsKeyButton btn keyBtn
                    in IM.traverseWithKey update keyBtnsMap

                mouseKbKeyBtnsMap = _mouseKbKeyButtonsMap controlsTab
                gamepadKeyBtnsMap = _gamepadKeyButtonsMap controlsTab
                showToggleBtnType = _showToggleButtonType controlsTab
            in do
                (mouseKbKeyBtnsMap', gamepadKeyBtnsMap') <- case showToggleBtnType of
                    MouseKbInputType -> (mouseKbKeyBtnsMap,) <$> updateKeyButtonsMap gamepadKeyBtnsMap
                    GamepadInputType -> (,gamepadKeyBtnsMap) <$> updateKeyButtonsMap mouseKbKeyBtnsMap

                let
                    showMouseKbBtn = _showMouseKbButton (controlsTab :: SettingsControlsTab)
                    showGamepadBtn = _showGamepadButton (controlsTab :: SettingsControlsTab)
                (showMouseKbBtn', showGamepadBtn') <- case showToggleBtnType of
                    MouseKbInputType ->
                        (,) <$>
                        updateButtonAndSelection ControlsShowToggleSubSelection showMouseKbBtn <*>
                        lift (updateButton ButtonInactiveStatus showGamepadBtn)
                    GamepadInputType ->
                        (,) <$>
                        lift (updateButton ButtonInactiveStatus showMouseKbBtn) <*>
                        updateButtonAndSelection ControlsShowToggleSubSelection showGamepadBtn

                showToggleBtnType' <- if
                    | _isInitialUpdate controlsTab           -> lift readInitialShowToggleButtonType
                    | _isPressed (showMouseKbBtn' :: Button) -> return GamepadInputType
                    | _isPressed (showGamepadBtn' :: Button) -> return MouseKbInputType
                    | otherwise                              -> lift $ processToggleViewMessages showToggleBtnType

                let restoreDefBtn = _restoreDefaultsButton (controlsTab :: SettingsControlsTab)
                restoreDefBtn'   <- updateButtonAndSelection ControlsRestoreDefaultsSubSelection restoreDefBtn
                when (_isPressed (restoreDefBtn' :: Button)) $
                    lift writeRestoreDefaultsMsgs

                notificationDisplayTxt      <- lift $ processNotificationMessages controlsTab
                rebindKeyInputDisplayTxt    <- lift $ updateInputDisplayText (_rebindKeyInputDisplayText controlsTab)
                clearKeyInputDisplayTxt     <- lift $ updateInputDisplayText (_clearKeyInputDisplayText controlsTab)
                cancelRebindInputDisplayTxt <-
                    lift $ updateInputDisplayText (_cancelRebindInputDisplayText controlsTab)

                let
                    isBtnPressed =
                        or $ map (_isPressed :: Button -> Bool) [showMouseKbBtn', showGamepadBtn', restoreDefBtn']
                    soundIndices = _soundIndices (settingsMenuData :: SettingsMenuData)
                when isBtnPressed $
                    void $ playFmodSound (_confirmSmall soundIndices)

                let
                    keyBtnBtns      = IM.elems . IM.map _button $ case showToggleBtnType' of
                        MouseKbInputType -> gamepadKeyBtnsMap'
                        GamepadInputType -> mouseKbKeyBtnsMap'
                    isRebindPressed = or $ map (_isPressed :: Button -> Bool) keyBtnBtns
                when isRebindPressed $
                    void $ playFmodSound (_confirmRebind soundIndices)

                get <&> \(select, subSelect) ->
                    let
                        controlsTab' = controlsTab
                            { _isInitialUpdate              = False
                            , _showToggleButtonType         = showToggleBtnType'
                            , _mouseKbKeyButtonsMap         = mouseKbKeyBtnsMap'
                            , _gamepadKeyButtonsMap         = gamepadKeyBtnsMap'
                            , _restoreDefaultsButton        = restoreDefBtn'
                            , _showMouseKbButton            = showMouseKbBtn'
                            , _showGamepadButton            = showGamepadBtn'
                            , _subSelection                 = subSelect
                            , _notificationDisplayText      = notificationDisplayTxt
                            , _rebindKeyInputDisplayText    = rebindKeyInputDisplayTxt
                            , _clearKeyInputDisplayText     = clearKeyInputDisplayTxt
                            , _cancelRebindInputDisplayText = cancelRebindInputDisplayTxt
                            }
                    in \smd -> smd
                        { _tabChoice   = tabChoice
                        , _tabButtons  = tabBtns'
                        , _controlsTab = controlsTab'
                        , _selection   = select
                        }

drawSettingsControlsTab :: (ConfigsRead m, GraphicsReadWrite m, InputRead m, MonadIO m) => SettingsControlsTab -> m ()
drawSettingsControlsTab controlsTab = do
    drawImage zeroPos2 RightDir menuOverZIndex (_backgroundImage (controlsTab :: SettingsControlsTab))
    when (_showToggleButtonType controlsTab == GamepadInputType) $
        drawImage zeroPos2 RightDir menuOverZIndex (_cursorLockOnOverlayImage controlsTab)

    let
        (controlsKeyBtnsMap, showToggleBtn) = case _showToggleButtonType controlsTab of
            MouseKbInputType -> (_gamepadKeyButtonsMap controlsTab, _showMouseKbButton controlsTab)
            GamepadInputType -> (_mouseKbKeyButtonsMap controlsTab, _showGamepadButton controlsTab)

    traverse_ drawControlsKeyButton (IM.elems controlsKeyBtnsMap)

    case _subSelection (controlsTab :: SettingsControlsTab) of
        ControlsKeyButtonSubSelection _
            | isControlsTabWaitingForInput controlsTab ->
                let cancelRebindDisplayTxt = _cancelRebindInputDisplayText controlsTab
                in drawInputDisplayTextCentered rebindPromptCenterTextPos menuOverZIndex cancelRebindDisplayTxt

            | otherwise -> do
                let
                    rebindKeyIDT = _rebindKeyInputDisplayText controlsTab
                    clearKeyIDT  = _clearKeyInputDisplayText controlsTab
                rebindKeyIDTWidth <- inputDisplayTextWidth rebindKeyIDT
                clearKeyIDTWidth  <- inputDisplayTextWidth clearKeyIDT

                let
                    totalWidth       = rebindKeyIDTWidth + rebindPromptSpacerWidth + clearKeyIDTWidth
                    rebindKeyTextX   = rebindPromptCenterTextX - totalWidth / 2.0 + rebindKeyIDTWidth / 2.0
                    rebindKeyTextPos = Pos2 rebindKeyTextX rebindPromptCenterTextY
                    clearKeyTextX    = rebindPromptCenterTextX + totalWidth / 2.0 - clearKeyIDTWidth / 2.0
                    clearKeyTextPos  = Pos2 clearKeyTextX rebindPromptCenterTextY

                drawInputDisplayTextCentered rebindKeyTextPos menuOverZIndex rebindKeyIDT
                drawInputDisplayTextCentered clearKeyTextPos menuOverZIndex clearKeyIDT

        _ -> return ()

    drawDisplayTextCentered notificationTextPos menuOverZIndex (_notificationDisplayText controlsTab)

    drawButton menuOverZIndex showToggleBtn
    drawButton menuOverZIndex (_restoreDefaultsButton (controlsTab :: SettingsControlsTab))

isControlsTabWaitingForInput :: SettingsControlsTab -> Bool
isControlsTabWaitingForInput controlsTab = or $ map _isWaitingForInput (IM.elems keyBtnsMap)
    where
        keyBtnsMap = case _showToggleButtonType controlsTab of
            MouseKbInputType -> _gamepadKeyButtonsMap controlsTab
            GamepadInputType -> _mouseKbKeyButtonsMap controlsTab
