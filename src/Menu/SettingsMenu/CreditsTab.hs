module Menu.SettingsMenu.CreditsTab
    ( mkSettingsCreditsTab
    , updateSettingsCreditsTab
    , drawSettingsCreditsTab
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import FileCache
import Menu.SettingsMenu.CreditsTab.Types
import Menu.SettingsMenu.Types
import Menu.SettingsMenu.Util
import Menu.ZIndex
import Util
import Window.Graphics
import Window.Graphics.UiControls
import Window.InputState

settingsMenuPack            = \p -> PackResourceFilePath "data/menu/settings-menu.pack" p
controlsBtnImgPath          = settingsMenuPack "controls-button-inactive.image" :: PackResourceFilePath
graphicsBtnImgPath          = settingsMenuPack "graphics-button-inactive.image" :: PackResourceFilePath
audioBtnImgPath             = settingsMenuPack "audio-button-inactive.image"    :: PackResourceFilePath
creditsBtnImgPath           = settingsMenuPack "credits-button.image"           :: PackResourceFilePath
creditsTabBackgroundImgPath = settingsMenuPack "credits-background.image"       :: PackResourceFilePath

mkSettingsCreditsTab :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m SettingsCreditsTab
mkSettingsCreditsTab = do
    tabBtns       <- mkSettingsTabButtons controlsBtnImgPath graphicsBtnImgPath audioBtnImgPath creditsBtnImgPath
    backgroundImg <- loadPackImage creditsTabBackgroundImgPath

    return $ SettingsCreditsTab
        { _buttons         = tabBtns
        , _backgroundImage = backgroundImg
        }

updateSelections :: InputRead m => SettingsMenuSelection -> m SettingsMenuSelection
updateSelections selection = do
    inputState <- readInputState
    let
        upPressed    = MenuUpAlias `aliasPressed` inputState
        downPressed  = MenuDownAlias `aliasPressed` inputState

    return $ case selection of
        SettingsMenuControlsTabSelection
            | upPressed || downPressed -> SettingsMenuCloseSelection
        SettingsMenuGraphicsTabSelection
            | upPressed || downPressed -> SettingsMenuCloseSelection
        SettingsMenuAudioTabSelection
            | upPressed || downPressed -> SettingsMenuCloseSelection
        SettingsMenuCreditsTabSelection
            | upPressed || downPressed -> SettingsMenuCloseSelection
        SettingsMenuCloseSelection
            | upPressed || downPressed -> SettingsMenuCreditsTabSelection
        _                              -> selection

updateTabButtons :: SettingsMenuData -> SettingsTabButtons -> (SettingsTabChoice, SettingsTabButtons)
updateTabButtons settingsMenuData tabBtns
    | _isPressed (_controlsButton tabBtns :: Button) =
        ( ControlsTabChoice
        , _buttons (_controlsTab settingsMenuData :: SettingsControlsTab)
        )
    | _isPressed (_graphicsButton tabBtns :: Button) =
        ( GraphicsTabChoice
        , _buttons (_graphicsTab settingsMenuData :: SettingsGraphicsTab)
        )
    | _isPressed (_audioButton tabBtns :: Button)    =
        ( AudioTabChoice
        , _buttons (_audioTab settingsMenuData :: SettingsAudioTab)
        )
    | otherwise                                      = (CreditsTabChoice, tabBtns)

updateSettingsCreditsTab
    :: InputRead m
    => SettingsMenuSelection
    -> SettingsTabButtons
    -> SettingsMenuData
    -> m (SettingsMenuData -> SettingsMenuData)
updateSettingsCreditsTab selection tabBtns settingsMenuData = do
    let (tabChoice, tabBtns') = updateTabButtons settingsMenuData tabBtns
    selection'               <- updateSelections selection

    return $ \smd -> smd
        { _tabChoice  = tabChoice
        , _tabButtons = tabBtns'
        , _selection  = selection'
        }

drawSettingsCreditsTab :: (GraphicsReadWrite m, MonadIO m) => SettingsCreditsTab -> m ()
drawSettingsCreditsTab creditsTab = drawImage zeroPos2 RightDir menuOverZIndex backgroundImg
    where backgroundImg = _backgroundImage (creditsTab :: SettingsCreditsTab)
