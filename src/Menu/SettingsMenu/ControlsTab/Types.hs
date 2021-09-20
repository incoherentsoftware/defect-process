module Menu.SettingsMenu.ControlsTab.Types
    ( ControlsKeyButton(..)
    , ControlsSubSelection(..)
    , SettingsControlsTab(..)
    ) where

import qualified Data.IntMap as IM

import Menu.SettingsMenu.ControlsTab.KeyButtons
import Menu.SettingsMenu.Util
import Window.Graphics
import Window.Graphics.UiControls
import Window.InputState

data ControlsSubSelection
    = ControlsNoSubSelection
    | ControlsKeyButtonSubSelection Int
    | ControlsShowToggleSubSelection
    | ControlsRestoreDefaultsSubSelection
    deriving Eq

data SettingsControlsTab = SettingsControlsTab
    { _buttons                      :: SettingsTabButtons
    , _backgroundImage              :: Image
    , _isInitialUpdate              :: Bool
    , _showToggleButtonType         :: InputType
    , _mouseKbKeyButtonsMap         :: IM.IntMap ControlsKeyButton
    , _gamepadKeyButtonsMap         :: IM.IntMap ControlsKeyButton
    , _showMouseKbButton            :: Button
    , _showGamepadButton            :: Button
    , _restoreDefaultsButton        :: Button
    , _subSelection                 :: ControlsSubSelection
    , _notificationDisplayText      :: DisplayText
    , _rebindKeyInputDisplayText    :: InputDisplayText
    , _clearKeyInputDisplayText     :: InputDisplayText
    , _cancelRebindInputDisplayText :: InputDisplayText
    }
