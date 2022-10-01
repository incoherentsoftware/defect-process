module Configs.All.Enemy.Flying
    ( FlyingEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Enemy.DeathEffectData.Types
import Enemy.HurtEffectData.Types
import Enemy.SpawnEffectData.Types
import Util
import Window.Graphics.Util

data FlyingEnemyConfig = FlyingEnemyConfig
    { _health :: Health
    , _width  :: Float
    , _height :: Float

    , _initialAttackCooldown  :: Secs
    , _shootDistanceX         :: Distance
    , _shootReleaseFrameIndex :: FrameIndex
    , _shootOffsetPos         :: Pos2
    , _attackProjectileSpeed  :: Speed
    , _idleAttackFrameIndex   :: FrameIndex
    , _shockRange             :: Distance
    , _shootCooldown          :: Secs
    , _shockCooldown          :: Secs

    , _riseRecoverVelY        :: VelY
    , _idleSecs               :: Secs
    , _getUpUprightFrameIndex :: FrameIndex
    , _staggerThreshold        :: Stagger

    , _groundImpactEffectDrawScale :: DrawScale
    , _wallImpactEffectDrawScale   :: DrawScale

    , _hurtEffectData  :: EnemyHurtEffectData
    , _deathEffectData :: EnemyDeathEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON FlyingEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
