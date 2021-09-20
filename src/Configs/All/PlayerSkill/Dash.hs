module Configs.All.PlayerSkill.Dash
    ( DashConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util

data DashConfig = DashConfig
    { _dashSpeed         :: Speed
    , _dashGroundGravity :: Float
    , _dashCooldown      :: Secs
    }
    deriving Generic

instance FromJSON DashConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
