module Configs.All.PlayerSkill.SummonPlatform
    ( SummonPlatformConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Player.Meter
import Util

data SummonPlatformConfig = SummonPlatformConfig
    { _summonCooldown      :: Secs
    , _summonOffset        :: Pos2
    , _summonSoftVelY      :: VelY
    , _summonMeterCost     :: MeterValue
    , _platformTimeoutSecs :: Secs
    }
    deriving Generic

instance FromJSON SummonPlatformConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
