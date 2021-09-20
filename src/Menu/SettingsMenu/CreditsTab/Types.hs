module Menu.SettingsMenu.CreditsTab.Types
    ( SettingsCreditsTab(..)
    ) where

import Menu.SettingsMenu.Util
import Window.Graphics

data SettingsCreditsTab = SettingsCreditsTab
    { _buttons         :: SettingsTabButtons
    , _backgroundImage :: Image
    }
