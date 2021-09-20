module Configs.All.Enemy.Hop
    ( HopEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Util
import Window.Graphics.Util
import {-# SOURCE #-} Enemy.Util

data HopEnemyConfig = HopEnemyConfig
    { _health :: Health
    , _width  :: Float
    , _height :: Float

    , _idleSecs                      :: Secs
    , _staggerThreshold              :: Stagger
    , _hopProjCooldown               :: Secs
    , _hopLongReleaseProjOffset      :: Pos2
    , _hopShortLandReleaseProjOffset :: Pos2
    , _hopLongVel                    :: Vel2
    , _hopShortVel                   :: Vel2

    , _groundImpactEffectDrawScale :: DrawScale
    , _wallImpactEffectDrawScale   :: DrawScale

    , _hurtEffectData  :: EnemyHurtEffectData
    , _deathEffectData :: EnemyDeathEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON HopEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
