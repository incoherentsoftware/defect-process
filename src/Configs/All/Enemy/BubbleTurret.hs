module Configs.All.Enemy.BubbleTurret
    ( BubbleTurretEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Util
import {-# SOURCE #-} Enemy.Util

data BubbleTurretEnemyConfig = BubbleTurretEnemyConfig
    { _health :: Health
    , _width  :: Float
    , _height :: Float

    , _aggroRange               :: Distance
    , _turnAroundTimerSecs      :: Secs
    , _turnAroundAttackCooldown :: Secs
    , _initialAttackCooldown    :: Secs
    , _bubbleAttackCooldown     :: Secs
    , _bubbleAttackMouthOffset  :: Pos2

    , _bubbleProjWidth              :: Float
    , _bubbleProjHeight             :: Float
    , _bubbleProjAliveSecs          :: Secs
    , _bubbleProjSpeedX             :: Speed
    , _bubbleProjSpeedY             :: Speed
    , _bubbleProjInitialRiseSecs    :: Secs
    , _bubbleProjRiseFallPeriodSecs :: Secs
    , _staggerThreshold             :: Stagger

    , _hurtEffectData  :: EnemyHurtEffectData
    , _deathEffectData :: EnemyDeathEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON BubbleTurretEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
