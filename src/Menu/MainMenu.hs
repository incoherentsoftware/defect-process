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

mainMenuPack               = \p -> PackResourceFilePath "data/menu/main-menu.pack" p
newGameButtonImgPath       = mainMenuPack "new-game-button.image"        :: PackResourceFilePath
continueButtonImgPath      = mainMenuPack "continue-button.image"        :: PackResourceFilePath
unlocksButtonImgPath       = mainMenuPack "unlocks-button.image"         :: PackResourceFilePath
settingsButtonImgPath      = mainMenuPack "settings-button.image"        :: PackResourceFilePath
quitButtonImgPath          = mainMenuPack "quit-button.image"            :: PackResourceFilePath
quitPromptImgPath          = mainMenuPack "prompt.image"                 :: PackResourceFilePath
newGamePromptImgPath       = mainMenuPack "new-game-prompt.image"        :: PackResourceFilePath
promptQuitButtonImgPath    = mainMenuPack "prompt-quit-button.image"     :: PackResourceFilePath
promptCancelButtonImgPath  = mainMenuPack "prompt-cancel-button.image"   :: PackResourceFilePath
promptNewGameButtonImgPath = mainMenuPack "prompt-new-game-button.image" :: PackResourceFilePath
bgImagePath                = mainMenuPack "main-menu-background.image"   :: PackResourceFilePath
splashOverlayImgPath       = mainMenuPack "splash-overlay.image"         :: PackResourceFilePath

mkMainMenuData :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m) => m MainMenuData
mkMainMenuData = do
    settingsCfg      <- _settings <$> readConfigs
    splashOverlayImg <- if
        | _startingMode (_debug settingsCfg) == MainMenuMode -> Just <$> loadPackImage splashOverlayImgPath
        | otherwise                                          -> return Nothing

    let
        buttonX             = virtualRenderWidth / 2.0
        menuCfg             = _menu (settingsCfg :: SettingsConfig)
        newGameBtnPos       = Pos2 buttonX (_mainNewGameButtonPosY menuCfg)
        continueBtnPos      = _mainContinueButtonPos menuCfg
        unlocksBtnPos       = Pos2 buttonX (_mainUnlocksButtonPosY menuCfg)
        settingsBtnPos      = Pos2 buttonX (_mainSettingsButtonPosY menuCfg)
        quitBtnPos          = Pos2 buttonX (_mainQuitButtonPosY menuCfg)
        promptQuitBtnPos    = _mainPromptQuitButtonPos menuCfg
        promptCancelBtnPos  = _mainPromptCancelButtonPos menuCfg
        promptNewGameBtnPos = _mainPromptNewGameButtonPos menuCfg

    backgroundImg    <- loadPackImage bgImagePath
    newGameBtn       <- mkImageButtonCentered newGameBtnPos newGameButtonImgPath
    continueBtn      <- mkImageButtonCentered continueBtnPos continueButtonImgPath
    unlocksBtn       <- mkImageButtonCentered unlocksBtnPos unlocksButtonImgPath
    settingsBtn      <- mkImageButtonCentered settingsBtnPos settingsButtonImgPath
    quitBtn          <- mkImageButtonCentered quitBtnPos quitButtonImgPath
    quitPromptImg    <- loadPackImage quitPromptImgPath
    newGamePromptImg <- loadPackImage newGamePromptImgPath
    promptQuitBtn    <- mkImageButtonCentered promptQuitBtnPos promptQuitButtonImgPath
    promptCancelBtn  <- mkImageButtonCentered promptCancelBtnPos promptCancelButtonImgPath
    promptNewGameBtn <- mkImageButtonCentered promptNewGameBtnPos promptNewGameButtonImgPath
    settingsMenuData <- mkSettingsMenuData
    musicIndex       <- getFmodMusic menuMusicPath
    soundIndices     <- mkMenuSoundIndices

    return $ MainMenuData
        { _backgroundImage            = backgroundImg
        , _splashOverlayImage         = splashOverlayImg
        , _newGameButton              = newGameBtn
        , _continueButton             = continueBtn
        , _unlocksButton              = unlocksBtn
        , _settingsButton             = settingsBtn
        , _quitButton                 = quitBtn
        , _quitPromptImage            = quitPromptImg
        , _quitPromptQuitButton       = promptQuitBtn
        , _quitPromptCancelButton     = promptCancelBtn
        , _newGamePromptImage         = newGamePromptImg
        , _newGamePromptNewGameButton = promptNewGameBtn
        , _newGamePromptCancelButton  = promptCancelBtn
        , _settingsMenuData           = settingsMenuData
        , _selection                  = Nothing
        , _musicIndex                 = musicIndex
        , _soundIndices               = soundIndices
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
    Just MainMenuQuitPromptQuitSelection
        | leftPressed || rightPressed -> MainMenuQuitPromptCancelSelection
    Just MainMenuQuitPromptCancelSelection
        | leftPressed || rightPressed -> MainMenuQuitPromptQuitSelection
    Just MainMenuNewGamePromptNewGameSelection
        | leftPressed || rightPressed -> MainMenuNewGamePromptCancelSelection
    Just MainMenuNewGamePromptCancelSelection
        | leftPressed || rightPressed -> MainMenuNewGamePromptNewGameSelection
    Just selection                    -> selection
    where
        mainMenuData       = _mainMenuData $ _menu (game :: Game)
        settingsMenuActive = _active (_settingsMenuData mainMenuData)
        showContinue       = isShowContinueButton game
        upPressed          = MenuUpAlias `aliasPressed` inputState
        downPressed        = MenuDownAlias `aliasPressed` inputState
        leftPressed        = MenuLeftAlias `aliasPressed` inputState
        rightPressed       = MenuRightAlias `aliasPressed` inputState

isQuitPromptSelection :: MainMenuSelection -> Bool
isQuitPromptSelection = \case
    MainMenuQuitPromptQuitSelection   -> True
    MainMenuQuitPromptCancelSelection -> True
    _                                 -> False

isNewGamePromptSelection :: MainMenuSelection -> Bool
isNewGamePromptSelection = \case
    MainMenuNewGamePromptNewGameSelection -> True
    MainMenuNewGamePromptCancelSelection  -> True
    _                                     -> False

isPromptSelection :: MainMenuSelection -> Bool
isPromptSelection selection = isQuitPromptSelection selection || isNewGamePromptSelection selection

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
                        | isQuitPromptSelection selection    -> not $ isQuitPromptSelection btnSelection
                        | isNewGamePromptSelection selection -> not $ isNewGamePromptSelection btnSelection
                        | otherwise                          -> isPromptSelection btnSelection

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
            newGameBtn  <- updateButton' MainMenuNewGameSelection (_newGameButton mainMenuData)
            continueBtn <- updateButton' MainMenuContinueSelection (_continueButton mainMenuData)
            unlocksBtn  <- updateButton' MainMenuUnlocksSelection (_unlocksButton mainMenuData)
            settingsBtn <- updateButton' MainMenuSettingsSelection (_settingsButton mainMenuData)
            quitBtn     <- updateButton' MainMenuQuitSelection (_quitButton mainMenuData)

            quitPromptQuitBtn       <-
                updateButton' MainMenuQuitPromptQuitSelection (_quitPromptQuitButton mainMenuData)
            quitPromptCancelBtn     <-
                updateButton' MainMenuQuitPromptCancelSelection (_quitPromptCancelButton mainMenuData)
            newGamePromptNewGameBtn <-
                updateButton' MainMenuNewGamePromptNewGameSelection (_newGamePromptNewGameButton mainMenuData)
            newGamePromptCancelBtn  <-
                updateButton' MainMenuNewGamePromptCancelSelection (_newGamePromptCancelButton mainMenuData)

            when (isShowContinueButton game && _isPressed quitBtn) $
                put MainMenuQuitPromptQuitSelection
            when (isShowContinueButton game && _isPressed newGameBtn) $
                put MainMenuNewGamePromptNewGameSelection
            when (_isPressed quitPromptQuitBtn) $
                put MainMenuQuitSelection
            when (_isPressed newGamePromptCancelBtn) $
                put MainMenuNewGameSelection

            let menuOrBackPressed = MenuAlias `aliasPressed` inputState || MenuBackAlias `aliasPressed` inputState
            whenM (get <&> \selection -> isQuitPromptSelection selection && menuOrBackPressed) $
                put MainMenuQuitSelection
            whenM (get <&> \selection -> isNewGamePromptSelection selection && menuOrBackPressed) $
                put MainMenuNewGameSelection

            let settingsMenuData = _settingsMenuData mainMenuData
            settingsMenuData'   <- if
                | _isPressed settingsBtn -> return $ settingsMenuData {_active = True}
                | settingsMenuActive     -> lift $ updateSettingsMenuData settingsBtn settingsMenuData
                | otherwise              -> return settingsMenuData

            get <&> \selection -> mainMenuData
                { _splashOverlayImage         = splashOverlayImg
                , _newGameButton              = newGameBtn
                , _continueButton             = continueBtn
                , _unlocksButton              = unlocksBtn
                , _settingsButton             = settingsBtn
                , _quitButton                 = quitBtn
                , _quitPromptQuitButton       = quitPromptQuitBtn
                , _quitPromptCancelButton     = quitPromptCancelBtn
                , _newGamePromptNewGameButton = newGamePromptNewGameBtn
                , _newGamePromptCancelButton  = newGamePromptCancelBtn
                , _settingsMenuData           = settingsMenuData'
                , _selection                  = Just selection
                }

drawMainMenu :: (ConfigsRead m, GraphicsReadWrite m, InputRead m, MonadIO m) => Game -> m ()
drawMainMenu game =
    let
        mainMenuData  = _mainMenuData $ _menu (game :: Game)
        backgroundImg = _backgroundImage (mainMenuData :: MainMenuData)
        selection     = _selection (mainMenuData :: MainMenuData)
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

                when (maybe False isQuitPromptSelection selection) $ do
                    mainPromptImagePos <- readSettingsConfig _menu _mainPromptImagePos
                    drawImage mainPromptImagePos RightDir menuZIndex (_quitPromptImage mainMenuData)
                    drawButton menuZIndex (_quitPromptQuitButton mainMenuData)
                    drawButton menuZIndex (_quitPromptCancelButton mainMenuData)

                when (maybe False isNewGamePromptSelection selection) $ do
                    mainPromptImagePos <- readSettingsConfig _menu _mainPromptImagePos
                    drawImage mainPromptImagePos RightDir menuZIndex (_quitPromptImage mainMenuData)
                    drawImage mainPromptImagePos RightDir menuZIndex (_newGamePromptImage mainMenuData)
                    drawButton menuZIndex (_newGamePromptNewGameButton mainMenuData)
                    drawButton menuZIndex (_newGamePromptCancelButton mainMenuData)

        drawSettingsMenuData $ _settingsMenuData mainMenuData

mainMenuMain :: Game -> AppEnv BaseMsgsPhase Game
mainMenuMain game = do
    mainMenuData <- withMsgsPhase @MenuMsgsPhase (updateMainMenuData game)

    let
        selection                      =_selection (mainMenuData :: MainMenuData)
        isShowContinueButton'          = isShowContinueButton game
        newGamePressed                 = _isPressed $ _newGameButton mainMenuData
        newGamePromptNewGameBtnPressed = _isPressed $ _newGamePromptNewGameButton mainMenuData
        shouldNewGame                  =
            (newGamePressed && not isShowContinueButton') || newGamePromptNewGameBtnPressed

        continuePressed = isShowContinueButton' && _isPressed (_continueButton mainMenuData)
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
        selection'
            | gameMode == UnlocksMenuMode = Just MainMenuUnlocksSelection
            | gameMode /= MainMenuMode    = Nothing
            | otherwise                   = selection

        mainMenuData' = mainMenuData {_selection = selection'} :: MainMenuData
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
        quitBtnPressed           = _isPressed $ _quitButton mainMenuData
        quitPromptQuitBtnPressed = _isPressed $ _quitPromptQuitButton mainMenuData
        promptPressed            = or
            [ quitPromptQuitBtnPressed
            , _isPressed $ _quitPromptCancelButton mainMenuData
            , newGamePromptNewGameBtnPressed
            , _isPressed $ _newGamePromptCancelButton mainMenuData
            ]
    when (newGamePressed || continuePressed || unlocksPressed || settingsPressed || quitBtnPressed || promptPressed) $
        void $ playFmodSound (_confirm soundIndices)

    void $ playOrResumeFmodMusicMenu (_musicIndex (mainMenuData' :: MainMenuData))

    let
        isQuitFromButton = (quitBtnPressed && not (isShowContinueButton game)) || quitPromptQuitBtnPressed
        shouldQuit       = quitHotkeyPressed || isQuitFromButton || _quit game

    return $ game
        { _mode     = gameMode
        , _prevMode = if gameMode /= MainMenuMode then MainMenuMode else _prevMode game
        , _menu     = menu {_mainMenuData = mainMenuData'}
        , _world    = world'
        , _quit     = shouldQuit
        }
