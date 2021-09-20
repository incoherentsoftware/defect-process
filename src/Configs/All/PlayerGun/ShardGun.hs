module Configs.All.PlayerGun.ShardGun
    ( ShardGunConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Player.Meter
import Util

data ShardGunConfig = ShardGunConfig
    { _shootRange           :: Distance
    , _impaleShardPercent   :: Float
    , _impaleShardAliveSecs :: Secs
    , _muzzleFlashOffset    :: Pos2

    , _shotAliveSecs           :: Secs
    , _shotDamage              :: Damage
    , _shotStartShoulderOffset :: Pos2
    , _shotMeterCost           :: MeterValue
    }
    deriving Generic

instance FromJSON ShardGunConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
