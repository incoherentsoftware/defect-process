module Menu.MainMenu
    ( mkMainMenuData
    , drawMainMenu
    , mainMenuMain
    ) where

import Control.Monad          (void, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (StateT, evalStateT, get, lift, put)
import Data.Functor           ((<&>))
import Data.Maybe             (isJust)

import AppEnv
import Audio.Fmod
import Configs
import Configs.All.Settings
import Configs.All.Settings.Debug
import Configs.All.Settings.Menu
import Constants
import FileCache
import Game.Types
import Menu.MainMenu.Types
import Menu.SettingsMenu
import Menu.SoundIndices
import Menu.Types
import Menu.Util
import Menu.ZIndex
import Msg
import Util
import Window
import Window.Graphics.UiControls.Button
import World

mainMenuPack              = \p -> PackResourceFilePath "data/menu/main-menu.pack" p
newGameButtonImgPath      = mainMenuPack "new-game-button.image"      :: PackResourceFilePath
continueButtonImgPath     = mainMenuPack "continue-button.image"      :: PackResourceFilePath
unlocksButtonImgPath      = mainMenuPack "unlocks-button.image"       :: PackResourceFilePath
settingsButtonImgPath     = mainMenuPack "settings-button.image"      :: PackResourceFilePath
quitButtonImgPath         = mainMenuPack "quit-button.image"          :: PackResourceFilePath
promptImgPath             = mainMenuPack "prompt.image"               :: PackResourceFilePath
promptQuitButtonImgPath   = mainMenuPack "prompt-quit-button.image"   :: PackResourceFilePath
promptCancelButtonImgPath = mainMenuPack "prompt-cancel-button.image" :: PackResourceFilePath
bgImagePath               = mainMenuPack "main-menu-background.image" :: PackResourceFilePath
splashOverlayImgPath      = mainMenuPack "splash-overlay.image"       :: PackResourceFilePath

mkMainMenuData :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m) => m MainMenuData
mkMainMenuData = do
    settingsCfg      <- _settings <$> readConfigs
    splashOverlayImg <- if
        | _startingMode (_debug settingsCfg) == MainMenuMode -> Just <$> loadPackImage splashOverlayImgPath
        | otherwise                                          -> return Nothing

    let
        buttonX            = virtualRenderWidth / 2.0
        menuCfg            = _menu (settingsCfg :: SettingsConfig)
        newGameBtnPos      = Pos2 buttonX (_mainNewGameButtonPosY menuCfg)
        continueBtnPos     = _mainContinueButtonPos menuCfg
        unlocksBtnPos      = Pos2 buttonX (_mainUnlocksButtonPosY menuCfg)
        settingsBtnPos     = Pos2 buttonX (_mainSettingsButtonPosY menuCfg)
        quitBtnPos         = Pos2 buttonX (_mainQuitButtonPosY menuCfg)
        promptQuitBtnPos   = _mainPromptQuitButtonPos menuCfg
        promptCancelBtnPos = _mainPromptCancelButtonPos menuCfg

    backgroundImg    <- loadPackImage bgImagePath
    newGameBtn       <- mkImageButtonCentered newGameBtnPos newGameButtonImgPath
    continueBtn      <- mkImageButtonCentered continueBtnPos continueButtonImgPath
    unlocksBtn       <- mkImageButtonCentered unlocksBtnPos unlocksButtonImgPath
    settingsBtn      <- mkImageButtonCentered settingsBtnPos settingsButtonImgPath
    quitBtn          <- mkImageButtonCentered quitBtnPos quitButtonImgPath
    promptImg        <- loadPackImage promptImgPath
    promptQuitBtn    <- mkImageButtonCentered promptQuitBtnPos promptQuitButtonImgPath
    promptCancelBtn  <- mkImageButtonCentered promptCancelBtnPos promptCancelButtonImgPath
    settingsMenuData <- mkSettingsMenuData
    musicIndex       <- getFmodMusic menuMusicPath
    soundIndices     <- mkMenuSoundIndices

    return $ MainMenuData
        { _backgroundImage    = backgroundImg
        , _splashOverlayImage = splashOverlayImg
        , _newGameButton      = newGameBtn
        , _continueButton     = continueBtn
        , _unlocksButton      = unlocksBtn
        , _settingsButton     = settingsBtn
        , _quitButton         = quitBtn
        , _promptImage        = promptImg
        , _promptQuitButton   = promptQuitBtn
        , _promptCancelButton = promptCancelBtn
        , _settingsMenuData   = settingsMenuData
        , _selection          = Nothing
        , _musicIndex         = musicIndex
        , _soundIndices       = soundIndices
        }

isShowContinueButton :: Game -> Bool
isShowContinueButton game = worldStatus == WorldAliveStatus
    where worldStatus = _status (_world (game :: Game) :: World)

readSelection :: InputState -> Game -> MainMenuSelection
readSelection inputState game = case _selection (mainMenuData :: MainMenuData) of
    _
        | settingsMenuActive          -> MainMenuSettingsSelection
    Nothing
        | showContinue                -> MainMenuContinueSelection
        | otherwise                   -> MainMenuNewGameSelection
    Just MainMenuContinueSelection
        | upPressed                   -> MainMenuQuitSelection
        | downPressed                 -> MainMenuNewGameSelection
    Just MainMenuNewGameSelection
        | showContinue && upPressed   -> MainMenuContinueSelection
        | upPressed                   -> MainMenuQuitSelection
        | downPressed                 -> MainMenuUnlocksSelection
    Just MainMenuUnlocksSelection
        | upPressed                   -> MainMenuNewGameSelection
        | downPressed                 -> MainMenuSettingsSelection
    Just MainMenuSettingsSelection
        | upPressed                   -> MainMenuUnlocksSelection
        | downPressed                 -> MainMenuQuitSelection
    Just MainMenuQuitSelection
        | upPressed                   -> MainMenuSettingsSelection
        | showContinue && downPressed -> MainMenuContinueSelection
        | downPressed                 -> MainMenuNewGameSelection
    Just MainMenuPromptQuitSelection
        | leftPressed || rightPressed -> MainMenuPromptCancelSelection
    Just MainMenuPromptCancelSelection
        | leftPressed || rightPressed -> MainMenuPromptQuitSelection
    Just selection                    -> selection
    where
        mainMenuData       = _mainMenuData $ _menu (game :: Game)
        settingsMenuActive = _active (_settingsMenuData mainMenuData)
        showContinue       = isShowContinueButton game
        upPressed          = MenuUpAlias `aliasPressed` inputState
        downPressed        = MenuDownAlias `aliasPressed` inputState
        leftPressed        = MenuLeftAlias `aliasPressed` inputState
        rightPressed       = MenuRightAlias `aliasPressed` inputState

isPromptSelection :: MainMenuSelection -> Bool
isPromptSelection = \case
    MainMenuPromptQuitSelection   -> True
    MainMenuPromptCancelSelection -> True
    _                             -> False

updateMainMenuData
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsReadWrite MenuMsgsPhase m)
    => Game
    -> m MainMenuData
updateMainMenuData game = do
    inputState <- readInputState

    let
        mainMenuData                     = _mainMenuData $ _menu (game :: Game)
        (splashOverlayImg, splashActive) = case _splashOverlayImage mainMenuData of
            Nothing    -> (Nothing, False)
            overlayImg -> if
                | _prevMode game /= MainMenuMode -> (Nothing, False)
                | anyKeyPressed inputState       -> (Nothing, True)
                | otherwise                      -> (overlayImg, False)

    flip evalStateT (readSelection inputState game) $
        let
            settingsMenuActive = _active $ _settingsMenuData mainMenuData

            isButtonDisabled :: Monad m => MainMenuSelection -> StateT MainMenuSelection m Bool
            isButtonDisabled btnSelection = do
                selection <- get
                return $ if
                    | btnSelection == MainMenuContinueSelection && not (isShowContinueButton game) -> True
                    | otherwise                                                                    -> if
                        | isPromptSelection selection -> not $ isPromptSelection btnSelection
                        | otherwise                   -> isPromptSelection btnSelection

            updateButton' :: InputRead m => MainMenuSelection -> Button -> StateT MainMenuSelection m Button
            updateButton' btnSelection btn = do
                isDisabled <- isButtonDisabled btnSelection
                btnStatus  <- get <&> \selection -> if
                    | settingsMenuActive || isDisabled || splashActive -> ButtonInactiveStatus
                    | btnSelection == selection                        -> ButtonSelectedActiveStatus
                    | otherwise                                        -> ButtonActiveStatus
                btn'      <- lift $ updateButton btnStatus btn

                when (_isSelected btn' || _isPressed btn') $
                    put btnSelection
                return btn'
        in do
            newGameBtn      <- updateButton' MainMenuNewGameSelection (_newGameButton mainMenuData)
            continueBtn     <- updateButton' MainMenuContinueSelection (_continueButton mainMenuData)
            unlocksBtn      <- updateButton' MainMenuUnlocksSelection (_unlocksButton mainMenuData)
            settingsBtn     <- updateButton' MainMenuSettingsSelection (_settingsButton mainMenuData)
            quitBtn         <- updateButton' MainMenuQuitSelection (_quitButton mainMenuData)
            promptQuitBtn   <- updateButton' MainMenuPromptQuitSelection (_promptQuitButton mainMenuData)
            promptCancelBtn <- updateButton' MainMenuPromptCancelSelection (_promptCancelButton mainMenuData)

            let
                quitBtnPressed         = _isPressed $ _quitButton mainMenuData
                promptCancelBtnPressed = _isPressed $ _promptCancelButton mainMenuData
                menuOrBackPressed      = MenuAlias `aliasPressed` inputState || MenuBackAlias `aliasPressed` inputState
            when (isShowContinueButton game && quitBtnPressed) $
                put MainMenuPromptQuitSelection
            when promptCancelBtnPressed $
                put MainMenuQuitSelection
            whenM (get <&> \selection -> isPromptSelection selection && menuOrBackPressed) $
                put MainMenuQuitSelection

            let settingsMenuData = _settingsMenuData mainMenuData
            settingsMenuData'   <- if
                | _isPressed settingsBtn -> return $ settingsMenuData {_active = True}
                | settingsMenuActive     -> lift $ updateSettingsMenuData settingsBtn settingsMenuData
                | otherwise              -> return settingsMenuData

            get <&> \selection -> mainMenuData
                { _splashOverlayImage = splashOverlayImg
                , _newGameButton      = newGameBtn
                , _continueButton     = continueBtn
                , _unlocksButton      = unlocksBtn
                , _settingsButton     = settingsBtn
                , _quitButton         = quitBtn
                , _promptQuitButton   = promptQuitBtn
                , _promptCancelButton = promptCancelBtn
                , _settingsMenuData   = settingsMenuData'
                , _selection          = Just selection
                }

drawMainMenu :: (ConfigsRead m, GraphicsReadWrite m, InputRead m, MonadIO m) => Game -> m ()
drawMainMenu game =
    let
        mainMenuData       = _mainMenuData $ _menu (game :: Game)
        backgroundImg      = _backgroundImage (mainMenuData :: MainMenuData)
        isPromptSelection' = maybe False isPromptSelection (_selection (mainMenuData :: MainMenuData))
    in do
        cursorVisible <- (== MouseKbInputType) . _lastUsedInputType <$> readInputState
        showCursor cursorVisible
        setCameraSpace CameraScreenSpace

        drawImage zeroPos2 RightDir menuZIndex backgroundImg

        case _splashOverlayImage mainMenuData of
            Just splashImg -> drawImage zeroPos2 RightDir menuZIndex splashImg
            Nothing        -> do
                when (isShowContinueButton game) $
                    drawButton menuZIndex (_continueButton mainMenuData)
                drawButton menuZIndex (_newGameButton mainMenuData)
                drawButton menuZIndex (_unlocksButton mainMenuData)
                drawButton menuZIndex (_settingsButton mainMenuData)
                drawButton menuZIndex (_quitButton mainMenuData)

                when isPromptSelection' $ do
                    mainPromptImagePos <- readSettingsConfig _menu _mainPromptImagePos
                    drawImage mainPromptImagePos RightDir menuZIndex (_promptImage mainMenuData)
                    drawButton menuZIndex (_promptQuitButton mainMenuData)
                    drawButton menuZIndex (_promptCancelButton mainMenuData)

        drawSettingsMenuData $ _settingsMenuData mainMenuData

mainMenuMain :: Game -> AppEnv BaseMsgsPhase Game
mainMenuMain game = do
    mainMenuData <- withMsgsPhase @MenuMsgsPhase (updateMainMenuData game)

    let
        shouldNewGame   = _isPressed $ _newGameButton mainMenuData
        continuePressed = isShowContinueButton game && _isPressed (_continueButton mainMenuData)
        unlocksPressed  = _isPressed $ _unlocksButton mainMenuData
        settingsPressed = _isPressed $ _settingsButton mainMenuData

        gameMode
            | shouldNewGame   = WorldMode
            | continuePressed = PauseMenuMode
            | unlocksPressed  = UnlocksMenuMode
            | otherwise       = MainMenuMode

    let world = _world (game :: Game)
    world'   <- if
        | shouldNewGame -> withMsgsPhase @SetupMsgsPhase (resetWorld world)
        | otherwise     -> return world

    let
        selection
            | gameMode == UnlocksMenuMode = Just MainMenuUnlocksSelection
            | gameMode /= MainMenuMode    = Nothing
            | otherwise                   = _selection (mainMenuData :: MainMenuData)

        mainMenuData' = mainMenuData {_selection = selection} :: MainMenuData
        menu          = _menu (game :: Game)

    let
        prevSplashActive = isJust $ _splashOverlayImage (_mainMenuData menu)
        splashActive     = isJust $ _splashOverlayImage mainMenuData'
        soundIndices     = _soundIndices (mainMenuData' :: MainMenuData)
    when (prevSplashActive && not splashActive) $
        void $ playFmodSound (_anyKey soundIndices)

    let prevSettingsMenuData = _settingsMenuData . _mainMenuData $ _menu (game :: Game)
    quitHotkeyPressed       <- if
        | isSettingsMenuControlsTabWaitingForInput prevSettingsMenuData -> return False
        | otherwise                                                     -> isMenuQuitHotkeyPressed
    let
        quitBtnPressed       = _isPressed $ _quitButton mainMenuData
        promptQuitBtnPressed = _isPressed $ _promptQuitButton mainMenuData
        promptPressed        = promptQuitBtnPressed || _isPressed (_promptCancelButton mainMenuData)
    when (shouldNewGame || continuePressed || unlocksPressed || settingsPressed || quitBtnPressed || promptPressed) $
        void $ playFmodSound (_confirm soundIndices)

    void $ playOrResumeFmodMusicMenu (_musicIndex (mainMenuData' :: MainMenuData))

    let
        isQuitFromButton = (quitBtnPressed && not (isShowContinueButton game)) || promptQuitBtnPressed
        shouldQuit       = quitHotkeyPressed || isQuitFromButton || _quit game

    return $ game
        { _mode     = gameMode
        , _prevMode = if gameMode /= MainMenuMode then MainMenuMode else _prevMode game
        , _menu     = menu {_mainMenuData = mainMenuData'}
        , _world    = world'
        , _quit     = shouldQuit
        }
