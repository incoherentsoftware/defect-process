module Menu.SettingsMenu.GraphicsTab.Types
    ( GraphicsSubSelection(..)
    , SettingsGraphicsTab(..)
    ) where

import Menu.SettingsMenu.Util
import Window.Graphics
import Window.Graphics.UiControls

data GraphicsSubSelection
    = GraphicsNoSubSelection
    | GraphicsResolutionSubSelection
    | GraphicsDisplayModeSubSelection
    | GraphicsRestoreDefaultsSubSelection
    deriving Eq

data SettingsGraphicsTab = SettingsGraphicsTab
    { _buttons                      :: SettingsTabButtons
    , _backgroundImage              :: Image
    , _resolutionComboBox           :: ComboBox
    , _displayModeComboBox          :: ComboBox
    , _restoreDefaultsButton        :: Button
    , _disabledComboBoxOverlayImage :: Image
    , _subSelection                 :: GraphicsSubSelection
    , _windowDisplayIndex           :: Int
    }
