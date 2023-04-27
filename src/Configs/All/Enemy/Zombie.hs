module Configs.All.Enemy.Zombie
    ( ZombieEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Enemy.DeathEffectData.Types
import Enemy.HurtEffectData.Types
import Enemy.SpawnEffectData.Types
import Util
import Window.Graphics.Util

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

    , _tauntUnderlayDrawScale      :: DrawScale
    , _groundImpactEffectDrawScale :: DrawScale
    , _wallImpactEffectDrawScale   :: DrawScale

    , _hurtEffectData  :: EnemyHurtEffectData
    , _deathEffectData :: EnemyDeathEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON ZombieEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
