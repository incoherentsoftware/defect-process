module Configs.All.PlayerGun.SpikeGun
    ( SpikeGunConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Player.Meter
import Util

data SpikeGunConfig = SpikeGunConfig
    { _summonOffset            :: Pos2
    , _summonMeterCost         :: MeterValue
    , _barrageCooldown         :: Secs
    , _barrageSpikeSpeed       :: Speed
    , _barrageSpikeAliveSecs   :: Secs
    , _barrageMaxSpikes        :: Int
    , _barrageReleaseAliveSecs :: Secs
    }
    deriving Generic

instance FromJSON SpikeGunConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
