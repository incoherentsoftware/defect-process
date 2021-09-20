module Configs.All.PlayerWeapon.Scythe
    ( ScytheConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util

data ScytheConfig = ScytheConfig
    { _summonForwardsOffset     :: Pos2
    , _summonLongForwardsOffset :: Pos2
    , _summonUpwardsOffset      :: Pos2
    , _summonAwayOffset         :: Pos2
    , _summonAirDiagonalOffset  :: Pos2
    , _glowOscillateAmplitude   :: PosY
    , _glowOscillatePeriod      :: Secs
    , _showTrackingLine         :: Bool
    , _chargeHeldThresholdSecs  :: Secs
    , _recallSlashHitVel        :: Vel2
    }
    deriving Generic

instance FromJSON ScytheConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
