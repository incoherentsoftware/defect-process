module Level.Room.SpringLauncher.JSON
    ( SpringLauncherJSON(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util

data SpringLauncherJSON = SpringLauncherJSON
    { _pos :: Pos2
    }
    deriving Generic

instance FromJSON SpringLauncherJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
