module Configs.All.Enemy.Hammer
    ( HammerEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Enemy.DeathEffectData.Types
import Enemy.HurtEffectData.Types
import Enemy.SpawnEffectData.Types
import Util
import Window.Graphics.Util

data HammerEnemyConfig = HammerEnemyConfig
    { _health :: Health
    , _width  :: Float
    , _height :: Float

    , _idleSecs                 :: Secs
    , _attackMeteorRangeX       :: Distance
    , _attackSwingRangeX        :: Distance
    , _attackSwingRangeY        :: Distance
    , _patrolSpeed              :: Speed
    , _staggerThreshold         :: Stagger
    , _willUseTeleportChance    :: Float
    , _teleportOffsetX          :: PosX
    , _rerollPerPatrolLoopCount :: Int

    , _groundImpactEffectDrawScale :: DrawScale
    , _wallImpactEffectDrawScale   :: DrawScale

    , _hurtEffectData  :: EnemyHurtEffectData
    , _deathEffectData :: EnemyDeathEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON HammerEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
