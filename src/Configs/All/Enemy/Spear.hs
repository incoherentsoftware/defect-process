module Configs.All.Enemy.Spear
    ( SpearEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Util
import Window.Graphics.Util
import {-# SOURCE #-} Enemy.Util

data SpearEnemyConfig = SpearEnemyConfig
    { _health :: Health
    , _width  :: Float
    , _height :: Float

    , _idleSecs               :: Secs
    , _retreatSecs            :: Secs
    , _walkSpeed              :: Speed
    , _minWalkSecs            :: Secs
    , _maxWalkSecs            :: Secs
    , _initialAtkCooldownSecs :: Secs
    , _shoveAtkCooldownSecs   :: Secs
    , _throwAtkCooldownSecs   :: Secs
    , _shoveAtkRange          :: Distance
    , _throwAtkRange          :: Distance
    , _staggerThreshold       :: Stagger
    , _releaseSpearProjOffset :: Pos2

    , _spearProjSurfaceOffset :: Pos2
    , _spearProjSurfaceWidth  :: Float
    , _spearProjSurfaceHeight :: Float

    , _groundImpactEffectDrawScale :: DrawScale
    , _wallImpactEffectDrawScale   :: DrawScale

    , _hurtEffectData  :: EnemyHurtEffectData
    , _deathEffectData :: EnemyDeathEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON SpearEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
