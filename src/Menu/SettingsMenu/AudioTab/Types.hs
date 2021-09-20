module Menu.SettingsMenu.AudioTab.Types
    ( AudioSubSelection(..)
    , SettingsAudioTab(..)
    ) where

import Menu.SettingsMenu.Util
import Window.Graphics
import Window.Graphics.UiControls

data AudioSubSelection
    = AudioNoSubSelection
    | AudioSoundSubSelection
    | AudioMusicSubSelection
    | AudioRestoreDefaultsSubSelection
    deriving Eq

data SettingsAudioTab = SettingsAudioTab
    { _buttons               :: SettingsTabButtons
    , _backgroundImage       :: Image
    , _soundComboBox         :: ComboBox
    , _musicComboBox         :: ComboBox
    , _restoreDefaultsButton :: Button
    , _subSelection          :: AudioSubSelection
    }
