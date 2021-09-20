module Configs.All.PlayerGun.GrenadeLauncher
    ( GrenadeLauncherConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)
import qualified Data.Map as M
import qualified Data.Vector as V

import Player.Gun.FireDrawAngle
import Player.Gun.FireDrawState.LegsState.Types
import Player.Meter
import Util

data GrenadeLauncherConfig = GrenadeLauncherConfig
    { _fireDelaySecs            :: Secs
    , _shootRange               :: Distance
    , _shotAliveSecs            :: Secs
    , _muzzleFlashOffset        :: Pos2
    , _leadArmAngleMultiplier   :: Float
    , _shotStartShoulderOffsetY :: PosY
    , _shotMeterCost            :: MeterValue

    , _grenadeWidth   :: Float
    , _grenadeHeight  :: Float
    , _grenadeSpeed   :: Speed
    , _grenadeGravity :: Float

    , _grenadeExplosionLaunchRadius :: Float
    , _grenadeExplosionLaunchSpeed  :: Float

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

instance FromJSON GrenadeLauncherConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
