module Configs.All.Enemy.Turret
    ( TurretEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Enemy.DeathEffectData.Types
import Enemy.HurtEffectData.Types
import Enemy.SpawnEffectData.Types
import Util
import Window.Graphics.Util

data TurretEnemyConfig = TurretEnemyConfig
    { _health :: Health
    , _width  :: Float
    , _height :: Float

    , _aggroRange               :: Distance
    , _turnAroundTimerSecs      :: Secs
    , _turnAroundAttackCooldown :: Secs
    , _initialAttackCooldown    :: Secs
    , _postAttackCooldown       :: Secs

    , _attackMaxRangeX           :: Distance
    , _attackRangeY              :: Distance
    , _attackDamage              :: Damage
    , _attackMouthOffset         :: Pos2
    , _attackMouthMidBeamOffsetX :: PosX
    , _staggerThreshold          :: Stagger

    , _tauntedAttackCooldown    :: Secs
    , _tauntedUnderlayDrawScale :: DrawScale

    , _hurtEffectData  :: EnemyHurtEffectData
    , _deathEffectData :: EnemyDeathEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON TurretEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
