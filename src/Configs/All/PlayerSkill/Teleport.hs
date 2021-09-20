module Configs.All.PlayerSkill.Teleport
    ( TeleportConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util

data TeleportConfig = TeleportConfig
    { _blinkMaxDistance :: Distance
    , _blinkVel         :: Vel2
    , _blinkCooldown    :: Secs
    }
    deriving Generic

instance FromJSON TeleportConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
