module Configs.All.Enemy.Turret
    ( TurretEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Util
import {-# SOURCE #-} Enemy.Util

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

    , _hurtEffectData  :: EnemyHurtEffectData
    , _deathEffectData :: EnemyDeathEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON TurretEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
