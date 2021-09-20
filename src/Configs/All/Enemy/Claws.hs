module Configs.All.Enemy.Claws
    ( ClawsEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Util
import Window.Graphics.Util
import {-# SOURCE #-} Enemy.Util

data ClawsEnemyConfig = ClawsEnemyConfig
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

    , _initialAttackCooldown   :: Secs
    , _attackRangeY            :: Distance
    , _slashRangeX             :: Distance
    , _projectileRangeX        :: Distance
    , _projectileSpawnOffset   :: Pos2
    , _willUseProjectileChance :: Float
    , _willUseDashChance       :: Float
    , _idleSecs                :: Secs
    , _hurtSecs                :: Secs
    , _staggerThreshold        :: Stagger

    , _groundImpactEffectDrawScale :: DrawScale
    , _wallImpactEffectDrawScale   :: DrawScale

    , _hurtEffectData  :: EnemyHurtEffectData
    , _deathEffectData :: EnemyDeathEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON ClawsEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
