module Configs.All.PlayerGun.RicochetGun
    ( RicochetGunConfig(..)
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

data RicochetGunConfig = RicochetGunConfig
    { _shootRange        :: Distance
    , _maxBullets        :: Int
    , _fireAirVel        :: Vel2
    , _betweenShotsCd    :: Secs
    , _reloadCd          :: Secs
    , _autoReloadCd      :: Secs
    , _maxBounceRange    :: Distance
    , _muzzleFlashOffset :: Pos2
    , _handsOffset       :: Pos2

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

instance FromJSON RicochetGunConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
