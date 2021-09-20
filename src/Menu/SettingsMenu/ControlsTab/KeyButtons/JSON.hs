module Menu.SettingsMenu.ControlsTab.KeyButtons.JSON
    ( ControlsKeyButtonJSON(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util
import Window.InputState.Alias

data ControlsKeyButtonJSON = ControlsKeyButtonJSON
    { _inputAlias :: InputAlias
    , _pos        :: Pos2
    , _indices    :: (Int, Int, Int, Int, Int)
    }
    deriving Generic

instance FromJSON ControlsKeyButtonJSON where
    parseJSON value = genericParseJSON aesonFieldDropUnderscore value
