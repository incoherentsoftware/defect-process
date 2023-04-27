module Configs.All.Enemy.Flail
    ( FlailEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Enemy.DeathEffectData.Types
import Enemy.HurtEffectData.Types
import Enemy.SpawnEffectData.Types
import Util
import Window.Graphics.Util

data FlailEnemyConfig = FlailEnemyConfig
    { _health :: Health
    , _width  :: Float
    , _height :: Float

    , _idleSecs               :: Secs
    , _retreatSecs            :: Secs
    , _walkSpeed              :: Speed
    , _minWalkSecs            :: Secs
    , _maxWalkSecs            :: Secs
    , _staggerThreshold       :: Stagger
    , _initialAtkCooldownSecs :: Secs

    , _attackCooldownSecs        :: Secs
    , _attackForwardsRangeX      :: Distance
    , _attackDiagUpwardsRangeX   :: Distance
    , _attackDiagUpwardsMinDistY :: Distance
    , _attackUpwardsRangeX       :: Distance
    , _attackUpwardsMinDistY     :: Distance

    , _tauntUnderlayDrawScale      :: DrawScale
    , _groundImpactEffectDrawScale :: DrawScale
    , _wallImpactEffectDrawScale   :: DrawScale

    , _hurtEffectData  :: EnemyHurtEffectData
    , _deathEffectData :: EnemyDeathEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON FlailEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
