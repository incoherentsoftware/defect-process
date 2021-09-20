module Configs.All.Settings
    ( SettingsConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Configs.All.Settings.Audio
import Configs.All.Settings.Controls
import Configs.All.Settings.Debug
import Configs.All.Settings.Input
import Configs.All.Settings.Menu
import Configs.All.Settings.Render
import Configs.All.Settings.UI
import Util

data SettingsConfig = SettingsConfig
    { _debug    :: DebugConfig
    , _render   :: RenderConfig
    , _audio    :: AudioConfig
    , _input    :: InputConfig
    , _ui       :: UiConfig
    , _menu     :: MenuConfig
    , _controls :: ControlsConfig
    }
    deriving Generic

instance FromJSON SettingsConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
