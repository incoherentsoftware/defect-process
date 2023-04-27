module Configs.All.Enemy.Bomb
    ( BombEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Enemy.HurtEffectData.Types
import Enemy.SpawnEffectData.Types
import Util
import Window.Graphics.Util

data BombEnemyConfig = BombEnemyConfig
    { _health :: Health
    , _width  :: Float
    , _height :: Float

    , _idleSecs                   :: Secs
    , _staggerThreshold           :: Stagger
    , _minSearchTurns             :: Int
    , _searchTurnSecs             :: Secs
    , _sprintSpeed                :: Speed
    , _maxSprintSecs              :: Secs
    , _maxSprintWrongWayDistanceX :: Distance
    , _attackExplodeRange         :: Distance
    , _attackExplodeTimerSecs     :: Secs

    , _tauntUnderlayDrawScale      :: DrawScale
    , _groundImpactEffectDrawScale :: DrawScale
    , _wallImpactEffectDrawScale   :: DrawScale

    , _hurtEffectData  :: EnemyHurtEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON BombEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
