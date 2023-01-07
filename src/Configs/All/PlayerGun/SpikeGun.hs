module Configs.All.PlayerGun.SpikeGun
    ( SpikeGunConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Player.Meter
import Util
import Window.Graphics.Opacity

data SpikeGunConfig = SpikeGunConfig
    { _summonOffset              :: Pos2
    , _summonMeterCost           :: MeterValue
    , _barrageCooldown           :: Secs
    , _barrageSpikeSpeed         :: Speed
    , _barrageSpikeAliveSecs     :: Secs
    , _barrageMaxSpikes          :: Int
    , _barrageReleaseAliveSecs   :: Secs
    , _ringSpeed                 :: Float
    , _ringOpacity               :: Opacity
    , _ringSpikeAltAngleOffsets1 :: [Radians]
    , _ringSpikeAltAngleOffsets2 :: [Radians]
    , _ringSpikeAltAngleOffsets3 :: [Radians]
    , _ringSpikeAltAngleOffsets4 :: [Radians]
    , _ringSpikeAltAngleOffsets5 :: [Radians]
    , _spikeAltSpeed             :: Float
    , _spikeAltOffset            :: Float
    , _spikeAltDamage            :: Damage
    }
    deriving Generic

instance FromJSON SpikeGunConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
