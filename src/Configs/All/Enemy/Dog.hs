module Configs.All.Enemy.Dog
    ( DogEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Enemy.DeathEffectData.Types
import Enemy.HurtEffectData.Types
import Enemy.SpawnEffectData.Types
import Util
import Window.Graphics.Util

data DogEnemyConfig = DogEnemyConfig
    { _health :: Health
    , _width  :: Float
    , _height :: Float

    , _idleSecs         :: Secs
    , _paceSpeed        :: Speed
    , _paceSecs         :: Secs
    , _runSpeed         :: Speed
    , _fallbackRunSecs  :: Secs
    , _minRunFromSecs   :: Secs
    , _maxRunFromSecs   :: Secs
    , _staggerThreshold :: Stagger

    , _tauntedIdleSecs       :: Secs
    , _tauntedPaceSecs       :: Secs
    , _tauntedMaxRunFromSecs :: Secs

    , _initialAttackCooldown    :: Secs
    , _postAttackCooldown       :: Secs
    , _headbuttRangeX           :: Distance
    , _shootRangeX              :: Distance
    , _shootOffset              :: Pos2
    , _shootProjectileSpeed     :: Speed
    , _shootProjectileAngle     :: Radians
    , _shootProjectileGravity   :: Float
    , _shootProjectileAliveSecs :: Secs
    , _willUseProjectileChance  :: Float

    , _tauntUnderlayDrawScale      :: DrawScale
    , _groundImpactEffectDrawScale :: DrawScale
    , _wallImpactEffectDrawScale   :: DrawScale

    , _hurtEffectData  :: EnemyHurtEffectData
    , _deathEffectData :: EnemyDeathEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON DogEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
