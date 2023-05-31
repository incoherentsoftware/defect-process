module Configs.All.Enemy.BubbleTurret
    ( BubbleTurretEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Enemy.DeathEffectData.Types
import Enemy.HurtEffectData.Types
import Enemy.SpawnEffectData.Types
import Util
import Window.Graphics.Util

data BubbleTurretEnemyConfig = BubbleTurretEnemyConfig
    { _health :: Health
    , _width  :: Float
    , _height :: Float

    , _aggroRange                 :: Distance
    , _turnAroundTimerSecs        :: Secs
    , _turnAroundAttackCooldown   :: Secs
    , _initialAttackCooldown      :: Secs
    , _bubbleAttackCooldown       :: Secs
    , _bubbleAttackMouthOffset    :: Pos2
    , _tauntedTurnAroundTimerSecs :: Secs
    , _tauntedCooldownMultiplier  :: Float

    , _bubbleProjWidth              :: Float
    , _bubbleProjHeight             :: Float
    , _bubbleProjAliveSecs          :: Secs
    , _bubbleProjSpeedX             :: Speed
    , _bubbleProjSpeedY             :: Speed
    , _bubbleProjInitialRiseSecs    :: Secs
    , _bubbleProjRiseFallPeriodSecs :: Secs
    , _staggerThreshold             :: Stagger

    , _tauntUnderlayDrawScale :: DrawScale

    , _hurtEffectData  :: EnemyHurtEffectData
    , _deathEffectData :: EnemyDeathEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON BubbleTurretEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
