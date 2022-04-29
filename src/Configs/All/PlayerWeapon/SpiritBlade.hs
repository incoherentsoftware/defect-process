module Configs.All.PlayerWeapon.SpiritBlade
    ( SpiritBladeConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Player.Meter
import Util

data SpiritBladeConfig = SpiritBladeConfig
    { _spiritFormMeterCost          :: MeterValue
    , _spiritLargeSlashOffset       :: Pos2
    , _spiritLaunchSlashOffset      :: Pos2
    , _spiritBackwardsSlashOffset   :: Pos2
    , _spiritPillarBlastOffset      :: Pos2
    , _spiritAirBlowOffset          :: Pos2
    , _spiritAirCircularSlashOffset :: Pos2
    }
    deriving Generic

instance FromJSON SpiritBladeConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
