module Menu
    ( module Menu.MainMenu
    , module Menu.PauseMenu
    , module Menu.UnlocksMenu
    , Menu
    , mkMenu
    ) where

import Control.Monad.IO.Class (MonadIO)

import Async.Request
import Configs
import FileCache
import Menu.MainMenu
import Menu.PauseMenu
import Menu.Types
import Menu.UnlocksMenu
import Window

mkMenu :: (AsyncRequestWrite m, ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m) => m Menu
mkMenu =
    Menu <$>
    mkMainMenuData <*>
    mkPauseMenuData <*>
    mkUnlocksMenuData
