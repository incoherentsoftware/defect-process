module Configs.All.Enemy.Axe
    ( AxeEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Enemy.DeathEffectData.Types
import Enemy.HurtEffectData.Types
import Enemy.SpawnEffectData.Types
import Util
import Window.Graphics.Util

data AxeEnemyConfig = AxeEnemyConfig
    { _health         :: Health
    , _width          :: Float
    , _height         :: Float
    , _airHurtWidth   :: Float
    , _airHurtHeight  :: Float
    , _airHurtOffsetY :: PosY

    , _advanceSpeed   :: Speed
    , _minAdvanceSecs :: Secs
    , _maxAdvanceSecs :: Secs

    , _retreatSpeed   :: Speed
    , _minRetreatSecs :: Secs
    , _maxRetreatSecs :: Secs

    , _initialAttackCooldown :: Secs
    , _slashDistanceX        :: Distance
    , _lungeDistanceX        :: Distance
    , _attackDistanceY       :: Distance
    , _willUseLungeChance    :: Float
    , _idleSecs              :: Secs
    , _hurtSecs              :: Secs
    , _staggerThreshold      :: Stagger

    , _tauntUnderlayDrawScale      :: DrawScale
    , _groundImpactEffectDrawScale :: DrawScale
    , _wallImpactEffectDrawScale   :: DrawScale

    , _hurtEffectData  :: EnemyHurtEffectData
    , _deathEffectData :: EnemyDeathEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON AxeEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
