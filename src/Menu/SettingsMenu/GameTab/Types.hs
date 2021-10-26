module Menu.SettingsMenu.GameTab.Types
    ( GameSubSelection(..)
    , SettingsGameTab(..)
    ) where

import Menu.SettingsMenu.Util
import Window.Graphics
import Window.Graphics.UiControls

data GameSubSelection
    = GameNoSubSelection
    | GameEnemyHealthSubSelection
    | GamePauseMenuHintsSubSelection
    | GameRestoreDefaultsSubSelection
    deriving Eq

data SettingsGameTab = SettingsGameTab
    { _buttons                   :: SettingsTabButtons
    , _backgroundImage           :: Image
    , _enemyHealthDisplayText    :: DisplayText
    , _pauseMenuHintsDisplayText :: DisplayText
    , _enemyHealthComboBox       :: ComboBox
    , _pauseMenuHintsComboBox    :: ComboBox
    , _restoreDefaultsButton     :: Button
    , _subSelection              :: GameSubSelection
    }
