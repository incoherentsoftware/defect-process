module Configs.All.Enemy.Giant
    ( GiantEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Enemy.DeathEffectData.Types
import Enemy.HurtEffectData.Types
import Enemy.SpawnEffectData.Types
import Util
import Window.Graphics.Util

data GiantEnemyConfig = GiantEnemyConfig
    { _health  :: Health
    , _width   :: Float
    , _height  :: Float

    , _advanceSpeed   :: Speed
    , _minAdvanceSecs :: Secs
    , _maxAdvanceSecs :: Secs

    , _retreatSpeed   :: Speed
    , _minRetreatSecs :: Secs
    , _maxRetreatSecs :: Secs

    , _initialAttackCooldown :: Secs
    , _postAttackCooldown    :: Secs
    , _punchRangeX           :: Distance
    , _punchMinRangeYAbove   :: Distance
    , _smashRangeX           :: Distance
    , _smashRangeY           :: Distance
    , _idleSecs              :: Secs

    , _tauntUnderlayDrawScale :: DrawScale

    , _hurtEffectData  :: EnemyHurtEffectData
    , _deathEffectData :: EnemyDeathEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON GiantEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
