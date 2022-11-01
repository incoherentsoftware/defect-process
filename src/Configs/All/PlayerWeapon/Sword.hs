module Configs.All.PlayerWeapon.Sword
    ( SwordConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Enemy.LockOnReticleData
import Player.Meter
import Util

data SwordConfig = SwordConfig
    { _slash3AoeHitVel            :: Vel2
    , _summonAttackOrbOffset      :: Pos2
    , _summonAttackOrbCost        :: MeterValue
    , _attackOrbMaxActivations    :: Int
    , _attackOrbAliveSecs         :: Secs
    , _attackOrbAppearVelX        :: VelX
    , _attackOrbWidth             :: Float
    , _attackOrbHeight            :: Float
    , _attackOrbLockOnReticleData :: EnemyLockOnReticleData
    , _chargeHeldThresholdSecs    :: Secs
    , _chargeMeterCost            :: MeterValue
    }
    deriving Generic

instance FromJSON SwordConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
