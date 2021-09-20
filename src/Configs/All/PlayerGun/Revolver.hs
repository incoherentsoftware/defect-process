module Configs.All.PlayerGun.Revolver
    ( RevolverConfig(..)
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

data RevolverConfig = RevolverConfig
    { _shootRange        :: Distance
    , _maxBullets        :: Int
    , _fireAirVel        :: Vel2
    , _betweenShotsCd    :: Secs
    , _reloadCd          :: Secs
    , _autoReloadCd      :: Secs
    , _muzzleFlashOffset :: Pos2

    , _shotAliveSecs         :: Secs
    , _shotHitVel            :: Vel2
    , _shotDamage            :: Damage
    , _shotStagger           :: Stagger
    , _shotHitstunMultiplier :: Float
    , _shotMeterCost         :: MeterValue

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

instance FromJSON RevolverConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
