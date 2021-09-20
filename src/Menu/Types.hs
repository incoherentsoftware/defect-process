module Menu.Types
    ( Menu(..)
    ) where

import Menu.MainMenu.Types
import Menu.PauseMenu.Types
import Menu.UnlocksMenu.Types

data Menu = Menu
    { _mainMenuData    :: MainMenuData
    , _pauseMenuData   :: PauseMenuData
    , _unlocksMenuData :: UnlocksMenuData
    }
