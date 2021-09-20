module Menu.MainMenu
    ( mkMainMenuData
    , drawMainMenu
    , mainMenuMain
    ) where

import Control.Monad          (void, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (StateT, evalStateT, get, lift, put)
import Data.Foldable          (sequenceA_)
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

mainMenuPack          = \p -> PackResourceFilePath "data/menu/main-menu.pack" p
newGameButtonImgPath  = mainMenuPack "new-game-button.image"      :: PackResourceFilePath
continueButtonImgPath = mainMenuPack "continue-button.image"      :: PackResourceFilePath
unlocksButtonImgPath  = mainMenuPack "unlocks-button.image"       :: PackResourceFilePath
settingsButtonImgPath = mainMenuPack "settings-button.image"      :: PackResourceFilePath
quitButtonImgPath     = mainMenuPack "quit-button.image"          :: PackResourceFilePath
bgImagePath           = mainMenuPack "main-menu-background.image" :: PackResourceFilePath
splashOverlayImgPath  = mainMenuPack "splash-overlay.image"       :: PackResourceFilePath

mkMainMenuData :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m) => m MainMenuData
mkMainMenuData = do

    settingsCfg      <- _settings <$> readConfigs
    splashOverlayImg <- if
        | _startingMode (_debug settingsCfg) == MainMenuMode -> Just <$> loadPackImage splashOverlayImgPath
        | otherwise                                          -> return Nothing

    let
        buttonX        = virtualRenderWidth / 2.0
        menuCfg        = _menu (settingsCfg :: SettingsConfig)
        newGameBtnPos  = Pos2 buttonX (_mainNewGameButtonPosY menuCfg)
        continueBtnPos = _mainContinueButtonPos menuCfg
        unlocksBtnPos  = Pos2 buttonX (_mainUnlocksButtonPosY menuCfg)
        settingsBtnPos = Pos2 buttonX (_mainSettingsButtonPosY menuCfg)
        quitBtnPos     = Pos2 buttonX (_mainQuitButtonPosY menuCfg)

    backgroundImg     <- loadPackImage bgImagePath
    newGameBtn        <- mkImageButtonCentered newGameBtnPos newGameButtonImgPath
    continueBtn       <- mkImageButtonCentered continueBtnPos continueButtonImgPath
    unlocksBtn        <- mkImageButtonCentered unlocksBtnPos unlocksButtonImgPath
    settingsBtn       <- mkImageButtonCentered settingsBtnPos settingsButtonImgPath
    quitBtn           <- mkImageButtonCentered quitBtnPos quitButtonImgPath
    settingsMenuData  <- mkSettingsMenuData
    musicIndex        <- getFmodMusic menuMusicPath
    soundIndices      <- mkMenuSoundIndices

    return $ MainMenuData
        { _backgroundImage    = backgroundImg
        , _splashOverlayImage = splashOverlayImg
        , _newGameButton      = newGameBtn
        , _continueButton     = continueBtn
        , _unlocksButton      = unlocksBtn
        , _settingsButton     = settingsBtn
        , _quitButton         = quitBtn
        , _settingsMenuData   = settingsMenuData
        , _selection          = Nothing
        , _musicIndex         = musicIndex
        , _soundIndices       = soundIndices
        }

showContinueButton :: Game -> Bool
showContinueButton game = worldStatus == WorldAliveStatus
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
    Just selection                    -> selection
    where
        mainMenuData       = _mainMenuData $ _menu (game :: Game)
        settingsMenuActive = _active (_settingsMenuData mainMenuData)
        showContinue       = showContinueButton game
        upPressed          = MenuUpAlias `aliasPressed` inputState
        downPressed        = MenuDownAlias `aliasPressed` inputState

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
            settingsMenuActive    = _active $ _settingsMenuData mainMenuData
            isContinueAndDisabled = \select -> select == MainMenuContinueSelection && not (showContinueButton game)

            updateButton' :: InputRead m => MainMenuSelection -> Button -> StateT MainMenuSelection m Button
            updateButton' btnSelection btn = do
                btnStatus <- get <&> \selection -> if
                    | settingsMenuActive || splashActive -> ButtonInactiveStatus
                    | isContinueAndDisabled btnSelection -> ButtonInactiveStatus
                    | btnSelection == selection          -> ButtonSelectedActiveStatus
                    | otherwise                          -> ButtonActiveStatus
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
                , _settingsMenuData   = settingsMenuData'
                , _selection          = Just selection
                }

drawMainMenu :: (ConfigsRead m, GraphicsReadWrite m, InputRead m, MonadIO m) => Game -> m ()
drawMainMenu game =
    let
        mainMenuData  = _mainMenuData $ _menu (game :: Game)
        backgroundImg = _backgroundImage (mainMenuData :: MainMenuData)
        newGameBtn    = _newGameButton mainMenuData
        continueBtn   = _continueButton mainMenuData
        unlocksBtn    = _unlocksButton mainMenuData
        settingsBtn   = _settingsButton mainMenuData
        quitBtn       = _quitButton mainMenuData
    in do
        cursorVisible <- (== MouseKbInputType) . _lastUsedInputType <$> readInputState
        showCursor cursorVisible
        setCameraSpace CameraScreenSpace

        drawImage zeroPos2 RightDir menuZIndex backgroundImg

        when (showContinueButton game) $
            drawButton menuZIndex continueBtn
        drawButton menuZIndex newGameBtn
        drawButton menuZIndex unlocksBtn
        drawButton menuZIndex settingsBtn
        drawButton menuZIndex quitBtn

        sequenceA_ $ drawImage zeroPos2 RightDir menuZIndex <$> _splashOverlayImage mainMenuData

        drawSettingsMenuData $ _settingsMenuData mainMenuData

mainMenuMain :: Game -> AppEnv BaseMsgsPhase Game
mainMenuMain game = do
    mainMenuData <- withMsgsPhase @MenuMsgsPhase (updateMainMenuData game)

    let
        shouldNewGame   = _isPressed $ _newGameButton mainMenuData
        continuePressed = showContinueButton game && _isPressed (_continueButton mainMenuData)
        unlocksPressed  = _isPressed $ _unlocksButton mainMenuData
        settingsPressed = _isPressed $ _settingsButton mainMenuData

        gameMode
            | shouldNewGame   = WorldMode
            | continuePressed = PauseMenuMode
            | unlocksPressed  = UnlocksMenuMode
            | otherwise       = MainMenuMode

    let prevSettingsMenuData = _settingsMenuData . _mainMenuData $ _menu (game :: Game)
    quitHotkeyPressed       <- if
        | isSettingsMenuControlsTabWaitingForInput prevSettingsMenuData -> return False
        | otherwise                                                     -> isMenuQuitHotkeyPressed
    let
        quitBtnPressed = _isPressed $ _quitButton mainMenuData
        shouldQuit     = quitHotkeyPressed || quitBtnPressed || _quit game

    let world = _world (game :: Game)
    world'   <- if
        | shouldNewGame -> withMsgsPhase @SetupMsgsPhase (resetWorld world)
        | otherwise     -> return world

    let
        prevGameMode
            | gameMode /= MainMenuMode = MainMenuMode
            | otherwise                = _prevMode game

        selection
            | gameMode == UnlocksMenuMode = Just MainMenuUnlocksSelection
            | gameMode /= MainMenuMode    = Nothing
            | otherwise                   = _selection (mainMenuData :: MainMenuData)

        mainMenuData' = mainMenuData {_selection = selection} :: MainMenuData
        menu          = _menu (game :: Game)

        prevSplashActive = isJust $ _splashOverlayImage (_mainMenuData menu)
        splashActive     = isJust $ _splashOverlayImage mainMenuData'
        soundIndices     = _soundIndices (mainMenuData' :: MainMenuData)
    when (prevSplashActive && not splashActive) $
        void $ playFmodSound (_anyKey soundIndices)
    when (shouldNewGame || continuePressed || unlocksPressed || settingsPressed || quitBtnPressed) $
        void $ playFmodSound (_confirm soundIndices)

    void $ playOrResumeFmodMusicMenu (_musicIndex (mainMenuData' :: MainMenuData))

    return $ game
        { _mode     = gameMode
        , _prevMode = prevGameMode
        , _menu     = menu {_mainMenuData = mainMenuData'}
        , _world    = world'
        , _quit     = shouldQuit
        }
