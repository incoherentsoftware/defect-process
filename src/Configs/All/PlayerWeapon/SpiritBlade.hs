module Configs.All.PlayerWeapon.SpiritBlade
    ( SpiritBladeConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util

data SpiritBladeConfig = SpiritBladeConfig
    { _temp :: String
    }
    deriving Generic

instance FromJSON SpiritBladeConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
