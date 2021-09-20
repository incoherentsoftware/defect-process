module Menu.SettingsMenu.Types
    ( SettingsTabButtons(..)
    , SettingsTabChoice(..)
    , SettingsControlsTab(..)
    , SettingsGraphicsTab(..)
    , SettingsAudioTab(..)
    , SettingsCreditsTab(..)
    , SettingsMenuSelection(..)
    , SettingsMenuData(..)
    ) where

import Menu.SettingsMenu.AudioTab.Types
import Menu.SettingsMenu.ControlsTab.Types
import Menu.SettingsMenu.CreditsTab.Types
import Menu.SettingsMenu.GraphicsTab.Types
import Menu.SettingsMenu.Util
import Menu.SoundIndices.Types
import Window.Graphics.UiControls

data SettingsTabChoice
    = ControlsTabChoice
    | GraphicsTabChoice
    | AudioTabChoice
    | CreditsTabChoice
    deriving Eq

data SettingsMenuSelection
    = SettingsMenuCloseSelection
    | SettingsMenuControlsTabSelection
    | SettingsMenuGraphicsTabSelection
    | SettingsMenuAudioTabSelection
    | SettingsMenuCreditsTabSelection
    | SettingsMenuControlsSubSelection
    | SettingsMenuGraphicsSubSelection
    | SettingsMenuAudioSubSelection
    deriving Eq

data SettingsMenuData = SettingsMenuData
    { _active       :: Bool
    , _tabChoice    :: SettingsTabChoice
    , _tabButtons   :: SettingsTabButtons
    , _controlsTab  :: SettingsControlsTab
    , _graphicsTab  :: SettingsGraphicsTab
    , _audioTab     :: SettingsAudioTab
    , _creditsTab   :: SettingsCreditsTab
    , _closeButton  :: Button
    , _selection    :: SettingsMenuSelection
    , _soundIndices :: MenuSoundIndices
    }
