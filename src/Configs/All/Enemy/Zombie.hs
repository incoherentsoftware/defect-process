module Configs.All.Enemy.Zombie
    ( ZombieEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Util
import Window.Graphics.Util
import {-# SOURCE #-} Enemy.Util

data ZombieEnemyConfig = ZombieEnemyConfig
    { _health :: Health
    , _width  :: Float
    , _height :: Float

    , _idleSecs         :: Secs
    , _walkSpeed        :: Speed
    , _minWalkSecs      :: Secs
    , _maxWalkSecs      :: Secs
    , _staggerThreshold :: Stagger

    , _initialAtkCooldown    :: Secs
    , _postSpawnNoAttackSecs :: Secs
    , _minAtkCooldown        :: Secs
    , _maxAtkCooldown        :: Secs
    , _atkFallChance         :: Secs
    , _forceAtkRangeX        :: Distance
    , _forceAtkRangeY        :: Distance
    , _atkSpitProjOffset     :: Pos2

    , _groundImpactEffectDrawScale :: DrawScale
    , _wallImpactEffectDrawScale   :: DrawScale

    , _hurtEffectData  :: EnemyHurtEffectData
    , _deathEffectData :: EnemyDeathEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON ZombieEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
