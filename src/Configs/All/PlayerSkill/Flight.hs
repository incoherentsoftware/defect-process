module Configs.All.PlayerSkill.Flight
    ( FlightConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util

data FlightConfig = FlightConfig
    { _hoverSecs            :: Secs
    , _hoverVel             :: Vel2
    , _dashSpeedX           :: Speed
    , _dashSpeedY           :: Speed
    , _dashDecelerateSpeedX :: Speed
    , _dashDecelerateSpeedY :: Speed
    , _baseSpeed            :: Speed
    }
    deriving Generic

instance FromJSON FlightConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
