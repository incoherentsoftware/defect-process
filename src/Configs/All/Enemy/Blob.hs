module Configs.All.Enemy.Blob
    ( BlobEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Enemy.DeathEffectData.Types
import Enemy.HurtEffectData.Types
import Enemy.SpawnEffectData.Types
import Util
import Window.Graphics.Util

data BlobEnemyConfig = BlobEnemyConfig
    { _health :: Health
    , _width  :: Float
    , _height :: Float

    , _postSpawnIdleSecs  :: Secs
    , _idleSecs           :: Secs
    , _minAttackMoveLoops :: Int
    , _maxAttackMoveLoops :: Int
    , _staggerThreshold   :: Stagger

    , _groundImpactEffectDrawScale :: DrawScale
    , _wallImpactEffectDrawScale   :: DrawScale

    , _hurtEffectData  :: EnemyHurtEffectData
    , _deathEffectData :: EnemyDeathEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON BlobEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
