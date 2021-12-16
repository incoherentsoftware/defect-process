module Configs.All.PlayerGun
    ( PlayerGunConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Configs.All.PlayerGun.GrenadeLauncher
import Configs.All.PlayerGun.Revolver
import Configs.All.PlayerGun.RicochetGun
import Configs.All.PlayerGun.ShardGun
import Configs.All.PlayerGun.Shotgun
import Configs.All.PlayerGun.SpikeGun
import Util

data PlayerGunConfig = PlayerGunConfig
    { _grenadeLauncher :: GrenadeLauncherConfig
    , _revolver        :: RevolverConfig
    , _shardGun        :: ShardGunConfig
    , _shotgun         :: ShotgunConfig
    , _spikeGun        :: SpikeGunConfig
    , _ricochetGun     :: RicochetGunConfig
    }
    deriving Generic

instance FromJSON PlayerGunConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
