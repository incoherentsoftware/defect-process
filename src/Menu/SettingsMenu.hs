module Menu.SettingsMenu
    ( module Menu.SettingsMenu.Types
    , mkSettingsMenuData
    , updateSettingsMenuData
    , drawSettingsMenuData
    , isSettingsMenuControlsTabWaitingForInput
    ) where

import Control.Monad          (void, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (StateT, evalStateT, get, lift, put)
import Data.Functor           ((<&>))

import Audio.Fmod
import Configs
import Configs.All.Settings
import Configs.All.Settings.Menu
import FileCache
import Menu.SettingsMenu.AudioTab
import Menu.SettingsMenu.ControlsTab
import Menu.SettingsMenu.ControlsTab.Types
import Menu.SettingsMenu.CreditsTab
import Menu.SettingsMenu.GameTab
import Menu.SettingsMenu.GraphicsTab
import Menu.SettingsMenu.Types
import Menu.SoundIndices
import Menu.ZIndex
import Msg
import Util
import Window.Graphics
import Window.Graphics.UiControls.Button
import Window.InputState

settingsMenuPack        = \p -> PackResourceFilePath "data/menu/settings-menu.pack" p
closeButtonImgPath      = settingsMenuPack "close-button.image"      :: PackResourceFilePath
commonBackgroundImgPath = settingsMenuPack "common-background.image" :: PackResourceFilePath

mkSettingsMenuData :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m) => m SettingsMenuData
mkSettingsMenuData = do
    cfg         <- readConfig _settings (_menu :: SettingsConfig -> MenuConfig)
    closeButton <- mkImageButton (_settingsCloseButtonPos cfg) closeButtonImgPath

    controlsTab  <- mkSettingsControlsTab
    graphicsTab  <- mkSettingsGraphicsTab
    audioTab     <- mkSettingsAudioTab
    gameTab      <- mkSettingsGameTab
    creditsTab   <- mkSettingsCreditsTab
    commonBgImg  <- loadPackImage commonBackgroundImgPath
    soundIndices <- mkMenuSoundIndices

    return $ SettingsMenuData
        { _active                = False
        , _tabChoice             = ControlsTabChoice
        , _tabButtons            = _buttons (controlsTab :: SettingsControlsTab)
        , _controlsTab           = controlsTab
        , _graphicsTab           = graphicsTab
        , _gameTab               = gameTab
        , _audioTab              = audioTab
        , _creditsTab            = creditsTab
        , _commonBackgroundImage = commonBgImg
        , _closeButton           = closeButton
        , _selection             = SettingsMenuCloseSelection
        , _soundIndices          = soundIndices
        }

readSelection :: InputState -> SettingsMenuData -> SettingsMenuSelection
readSelection inputState settingsMenuData = case _selection settingsMenuData of
    _
        | not (_active settingsMenuData) -> SettingsMenuCloseSelection

    SettingsMenuControlsTabSelection
        | leftPressed  -> SettingsMenuCreditsTabSelection
        | rightPressed -> SettingsMenuGraphicsTabSelection

    SettingsMenuGraphicsTabSelection
        | leftPressed  -> SettingsMenuControlsTabSelection
        | rightPressed -> SettingsMenuAudioTabSelection

    SettingsMenuAudioTabSelection
        | leftPressed  -> SettingsMenuGraphicsTabSelection
        | rightPressed -> SettingsMenuGameTabSelection

    SettingsMenuGameTabSelection
        | leftPressed  -> SettingsMenuAudioTabSelection
        | rightPressed -> SettingsMenuCreditsTabSelection

    SettingsMenuCreditsTabSelection
        | leftPressed  -> SettingsMenuGameTabSelection
        | rightPressed -> SettingsMenuControlsTabSelection

    selection -> selection

    where
        leftPressed  = MenuLeftAlias `aliasPressed` inputState
        rightPressed = MenuRightAlias `aliasPressed` inputState

tabLeftRightInputPressedSelection :: InputState -> SettingsMenuData -> Maybe SettingsMenuSelection
tabLeftRightInputPressedSelection inputState settingsMenuData = case _tabChoice settingsMenuData of
    ControlsTabChoice
        | tabLeftPressed  -> Just SettingsMenuCreditsTabSelection
        | tabRightPressed -> Just SettingsMenuGraphicsTabSelection

    GraphicsTabChoice
        | tabLeftPressed  -> Just SettingsMenuControlsTabSelection
        | tabRightPressed -> Just SettingsMenuAudioTabSelection

    AudioTabChoice
        | tabLeftPressed  -> Just SettingsMenuGraphicsTabSelection
        | tabRightPressed -> Just SettingsMenuGameTabSelection

    GameTabChoice
        | tabLeftPressed  -> Just SettingsMenuAudioTabSelection
        | tabRightPressed -> Just SettingsMenuCreditsTabSelection

    CreditsTabChoice
        | tabLeftPressed  -> Just SettingsMenuGameTabSelection
        | tabRightPressed -> Just SettingsMenuControlsTabSelection

    _ -> Nothing

    where
        tabLeftPressed  = MenuTabLeftAlias `aliasPressed` inputState
        tabRightPressed = MenuTabRightAlias `aliasPressed` inputState

updateSettingsMenuData
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsReadWrite MenuMsgsPhase m)
    => Button
    -> SettingsMenuData
    -> m SettingsMenuData
updateSettingsMenuData settingsBtn settingsMenuData = do
    inputState          <- readInputState
    let initialSelection = readSelection inputState settingsMenuData

    flip evalStateT initialSelection $
        let
            tabChoice = _tabChoice settingsMenuData

            isAnyExpandedComboBox           =
                isSettingsAudioTabExpandedComboBox (_audioTab settingsMenuData) ||
                isSettingsGraphicsTabExpandedComboBox (_graphicsTab settingsMenuData) ||
                isSettingsGameTabExpandedComboBox (_gameTab settingsMenuData)
            isControlsTabAndWaitingForInput =
                tabChoice == ControlsTabChoice && isControlsTabWaitingForInput (_controlsTab settingsMenuData)
            isButtonSelectionBlocked        = isAnyExpandedComboBox || isControlsTabAndWaitingForInput
            tabLeftRightInputSelection      = tabLeftRightInputPressedSelection inputState settingsMenuData

            updateButtonAndSelection
                :: InputRead m1
                => SettingsMenuSelection
                -> Button
                -> StateT SettingsMenuSelection m1 Button
            updateButtonAndSelection = \btnSelection btn -> do
                btn' <- case tabLeftRightInputSelection of
                    Just tabLeftRightSelection
                        | tabLeftRightSelection == btnSelection && not isButtonSelectionBlocked ->
                            return $ setButtonSelectedPressed btn

                    _ -> do
                        btnStatus <- get <&> \selection -> if
                            | isButtonSelectionBlocked  -> ButtonInactiveStatus
                            | btnSelection == selection -> ButtonSelectedActiveStatus
                            | otherwise                 -> ButtonActiveStatus
                        lift $ updateButton btnStatus btn

                when (_isSelected btn' || _isPressed btn') $
                    put btnSelection
                return btn'

            soundIndices = _soundIndices (settingsMenuData :: SettingsMenuData)
        in do
            closeButton <- updateButtonAndSelection SettingsMenuCloseSelection (_closeButton settingsMenuData)
            tabButtons  <- do
                let tabBtns  = _tabButtons settingsMenuData
                controlsBtn <- updateButtonAndSelection SettingsMenuControlsTabSelection (_controlsButton tabBtns)
                graphicsBtn <- updateButtonAndSelection SettingsMenuGraphicsTabSelection (_graphicsButton tabBtns)
                audioBtn    <- updateButtonAndSelection SettingsMenuAudioTabSelection (_audioButton tabBtns)
                gameBtn     <- updateButtonAndSelection SettingsMenuGameTabSelection (_gameButton tabBtns)
                creditsBtn  <- updateButtonAndSelection SettingsMenuCreditsTabSelection (_creditsButton tabBtns)

                when (or (map _isPressed [controlsBtn, graphicsBtn, audioBtn, gameBtn, creditsBtn])) $
                    void $ playFmodSound (_confirmSmall soundIndices)

                return $ tabBtns
                    { _controlsButton = controlsBtn
                    , _graphicsButton = graphicsBtn
                    , _audioButton    = audioBtn
                    , _gameButton     = gameBtn
                    , _creditsButton  = creditsBtn
                    }

            updateTab <- get >>= \selection -> lift $ case tabChoice of
                ControlsTabChoice -> updateSettingsControlsTab selection tabButtons settingsMenuData
                GraphicsTabChoice -> updateSettingsGraphicsTab selection tabButtons settingsMenuData
                AudioTabChoice    -> updateSettingsAudioTab selection tabButtons settingsMenuData
                GameTabChoice     -> updateSettingsGameTab selection tabButtons settingsMenuData
                CreditsTabChoice  -> updateSettingsCreditsTab selection tabButtons settingsMenuData

            let
                menuOrBackAliasPressed =
                    MenuAlias `aliasPressed` inputState || MenuBackAlias `aliasPressed` inputState

                active
                    | isButtonSelectionBlocked = True
                    | _active settingsMenuData = not menuOrBackAliasPressed && not (_isPressed closeButton)
                    | otherwise                = _isPressed settingsBtn

            when (_isPressed closeButton) $
                void $ playFmodSound (_confirm soundIndices)

            return $ (updateTab settingsMenuData)
                { _active      = active
                , _closeButton = closeButton
                }

drawSettingsMenuData :: (ConfigsRead m, GraphicsReadWrite m, InputRead m, MonadIO m) => SettingsMenuData -> m ()
drawSettingsMenuData settingsMenuData = when (_active settingsMenuData) $ do
    drawImage zeroPos2 RightDir menuOverZIndex (_commonBackgroundImage settingsMenuData)
    case _tabChoice settingsMenuData of
        ControlsTabChoice -> drawSettingsControlsTab $ _controlsTab settingsMenuData
        GraphicsTabChoice -> drawSettingsGraphicsTab $ _graphicsTab settingsMenuData
        AudioTabChoice    -> drawSettingsAudioTab $ _audioTab settingsMenuData
        GameTabChoice     -> drawSettingsGameTab $ _gameTab settingsMenuData
        CreditsTabChoice  -> drawSettingsCreditsTab $ _creditsTab settingsMenuData

    let tabBtns = _tabButtons settingsMenuData
    drawButton menuOverZIndex (_controlsButton tabBtns)
    drawButton menuOverZIndex (_graphicsButton tabBtns)
    drawButton menuOverZIndex (_audioButton tabBtns)
    drawButton menuOverZIndex (_gameButton tabBtns)
    drawButton menuOverZIndex (_creditsButton tabBtns)
    drawButton menuOverZIndex (_closeButton settingsMenuData)

isSettingsMenuControlsTabWaitingForInput :: SettingsMenuData -> Bool
isSettingsMenuControlsTabWaitingForInput settingsMenuData = case _tabChoice settingsMenuData of
    ControlsTabChoice -> isControlsTabWaitingForInput $ _controlsTab settingsMenuData
    _                 -> False
