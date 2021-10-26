module Menu.SettingsMenu.Types
    ( SettingsTabButtons(..)
    , SettingsTabChoice(..)
    , SettingsControlsTab(..)
    , SettingsGameTab(..)
    , SettingsGraphicsTab(..)
    , SettingsAudioTab(..)
    , SettingsCreditsTab(..)
    , SettingsMenuSelection(..)
    , SettingsMenuData(..)
    ) where

import Menu.SettingsMenu.AudioTab.Types
import Menu.SettingsMenu.ControlsTab.Types
import Menu.SettingsMenu.CreditsTab.Types
import Menu.SettingsMenu.GameTab.Types
import Menu.SettingsMenu.GraphicsTab.Types
import Menu.SettingsMenu.Util
import Menu.SoundIndices.Types
import Window.Graphics
import Window.Graphics.UiControls

data SettingsTabChoice
    = ControlsTabChoice
    | GraphicsTabChoice
    | AudioTabChoice
    | GameTabChoice
    | CreditsTabChoice
    deriving Eq

data SettingsMenuSelection
    = SettingsMenuCloseSelection
    | SettingsMenuControlsTabSelection
    | SettingsMenuGraphicsTabSelection
    | SettingsMenuAudioTabSelection
    | SettingsMenuGameTabSelection
    | SettingsMenuCreditsTabSelection
    | SettingsMenuControlsSubSelection
    | SettingsMenuGraphicsSubSelection
    | SettingsMenuAudioSubSelection
    | SettingsMenuGameSubSelection
    deriving Eq

data SettingsMenuData = SettingsMenuData
    { _active                :: Bool
    , _tabChoice             :: SettingsTabChoice
    , _tabButtons            :: SettingsTabButtons
    , _controlsTab           :: SettingsControlsTab
    , _graphicsTab           :: SettingsGraphicsTab
    , _audioTab              :: SettingsAudioTab
    , _gameTab               :: SettingsGameTab
    , _creditsTab            :: SettingsCreditsTab
    , _commonBackgroundImage :: Image
    , _closeButton           :: Button
    , _selection             :: SettingsMenuSelection
    , _soundIndices          :: MenuSoundIndices
    }
