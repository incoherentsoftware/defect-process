module Configs.All.PlayerWeapon.Staff
    ( StaffConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)
import qualified Data.Vector as V

import Player.Meter
import Util
import World.Screenshake.Types

data StaffConfig = StaffConfig
    { _bounceVelsY                     :: V.Vector VelY
    , _bounceScreenshakeMagnitude      :: ScreenshakeMagnitude
    , _groundStrikeOffsetX             :: PosX
    , _launchProjectileWidth           :: Float
    , _launchProjectileHeight          :: Float
    , _windProjectileMeterCost         :: MeterValue
    , _windProjectileMinCollisionDiffY :: Float
    , _chargeHeldThresholdSecs         :: Secs
    , _chargeMeterCost                 :: MeterValue
    }
    deriving Generic

instance FromJSON StaffConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
