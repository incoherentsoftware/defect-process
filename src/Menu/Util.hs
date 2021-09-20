module Menu.Util
    ( menuOptionBtnColor
    , hoverMenuOptionBtnColor
    , menuMusicPath
    , isMenuQuitHotkeyPressed
    ) where

import Configs
import Configs.All.Settings
import Configs.All.Settings.Debug
import Window.Graphics
import Window.InputState

menuOptionBtnColor      = whiteColor          :: Color
hoverMenuOptionBtnColor = Color 215 216 0 255 :: Color

menuMusicPath = "data/music/main-menu.ogg" :: FilePath

isMenuQuitHotkeyPressed :: (InputRead m, ConfigsRead m) => m Bool
isMenuQuitHotkeyPressed = readSettingsConfig _debug _menuQuitHotkeyEnabled >>= \case
    True  -> aliasPressed MenuQuitHotkeyAlias <$> readInputState
    False -> return False
