module Configs.All.Enemy.Wall
    ( WallEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Enemy.DeathEffectData.Types
import Enemy.HurtEffectData.Types
import Enemy.SpawnEffectData.Types
import Util
import Window.Graphics.Util

data WallEnemyConfig = WallEnemyConfig
    { _health :: Health
    , _width  :: Float
    , _height :: Float

    , _idleSecs         :: Secs
    , _walkSpeed        :: Speed
    , _minWalkSecs      :: Secs
    , _maxWalkSecs      :: Secs
    , _staggerThreshold :: Stagger

    , _initialAttackCooldown       :: Secs
    , _releaseWallProjOffset       :: Pos2
    , _releaseWallProjCooldown     :: Secs
    , _walkPlayerDistanceThreshold :: Distance

    , _tauntedIdleSecs              :: Secs
    , _tauntedMaxWalkSecs           :: Secs
    , _tauntedAtkCooldownMultiplier :: Float

    , _tauntedUnderlayDrawScale    :: DrawScale
    , _groundImpactEffectDrawScale :: DrawScale
    , _wallImpactEffectDrawScale   :: DrawScale

    , _hurtEffectData  :: EnemyHurtEffectData
    , _deathEffectData :: EnemyDeathEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON WallEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
