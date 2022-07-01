module Configs.All.PlayerGun.Shotgun
    ( ShotgunConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)
import qualified Data.Map as M
import qualified Data.Vector as V

import Attack.Util
import Player.Gun.FireDrawAngle
import Player.Gun.FireDrawState.LegsState.Types
import Player.Meter
import Util

data ShotgunConfig = ShotgunConfig
    { _fireAirVel        :: Vel2
    , _shootRange        :: Distance
    , _afterFireSecs     :: Secs
    , _pumpSecs          :: Secs
    , _muzzleFlashOffset :: Pos2

    , _shotgunSpreadDegrees :: [Degrees]

    , _shotAliveSecs           :: Secs
    , _shotDamage              :: Damage
    , _shotStartShoulderOffset :: Pos2
    , _shotMeterCost           :: MeterValue
    , _shotHitlag              :: Secs
    , _knockbackMagnitude      :: Float
    , _knockbackDistThreshold  :: Float

    , _burnShotDamage :: Damage
    , _burnShotHitlag :: Secs

    , _headAngleMultiplier     :: Float
    , _leadArmAngleMultiplier  :: Float
    , _rearArmAngleMultiplier  :: Float
    , _torsoAngleMultiplier    :: Float
    , _torsoNeckOffsets        :: M.Map GunFireDrawAngle Pos2
    , _rearShoulderHipsOffsets :: M.Map GunFireDrawAngle Pos2
    , _leadShoulderHipsOffsets :: M.Map GunFireDrawAngle Pos2
    , _legsHipsOffsets         :: M.Map LegsStatus (V.Vector Pos2)
    }
    deriving Generic

instance FromJSON ShotgunConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
