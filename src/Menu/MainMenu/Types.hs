module Menu.MainMenu.Types
    ( MainMenuSelection(..)
    , MainMenuData(..)
    ) where

import Audio.Fmod.Types
import Menu.SettingsMenu.Types
import Menu.SoundIndices.Types
import Window.Graphics
import Window.Graphics.UiControls

data MainMenuSelection
    = MainMenuNewGameSelection
    | MainMenuContinueSelection
    | MainMenuUnlocksSelection
    | MainMenuSettingsSelection
    | MainMenuQuitSelection
    | MainMenuQuitPromptQuitSelection
    | MainMenuQuitPromptCancelSelection
    | MainMenuNewGamePromptNewGameSelection
    | MainMenuNewGamePromptCancelSelection
    deriving Eq

data MainMenuData = MainMenuData
    { _backgroundImage            :: Image
    , _splashOverlayImage         :: Maybe Image
    , _newGameButton              :: Button
    , _continueButton             :: Button
    , _unlocksButton              :: Button
    , _settingsButton             :: Button
    , _quitButton                 :: Button
    , _quitPromptImage            :: Image
    , _quitPromptQuitButton       :: Button
    , _quitPromptCancelButton     :: Button
    , _newGamePromptImage         :: Image
    , _newGamePromptNewGameButton :: Button
    , _newGamePromptCancelButton  :: Button
    , _settingsMenuData           :: SettingsMenuData
    , _selection                  :: Maybe MainMenuSelection
    , _musicIndex                 :: FmodMusicIndex
    , _soundIndices               :: MenuSoundIndices
    }
