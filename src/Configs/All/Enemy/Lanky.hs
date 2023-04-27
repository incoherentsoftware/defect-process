module Configs.All.Enemy.Lanky
    ( LankyEnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
import Enemy.DeathEffectData.Types
import Enemy.HurtEffectData.Types
import Enemy.SpawnEffectData.Types
import Util
import Window.Graphics.Util
import World.Screenshake.Types

data LankyEnemyConfig = LankyEnemyConfig
    { _health  :: Health
    , _width   :: Float
    , _height  :: Float

    , _staggerThreshold       :: Stagger
    , _idleSecs               :: Secs
    , _walkSecs               :: Secs
    , _walkSpeed              :: Speed
    , _retreatSecs            :: Secs
    , _beamAtkRangeX          :: Distance
    , _summonAtkRangeX        :: Distance
    , _summonAtkCooldownSecs  :: Secs
    , _beamAtkCooldownSecs    :: Secs
    , _initialAtkCooldownSecs :: Secs

    , _maxAuraHealth                 :: Health
    , _auraBreakScreenshakeMagnitude :: ScreenshakeMagnitude

    , _tauntUnderlayDrawScale      :: DrawScale
    , _groundImpactEffectDrawScale :: DrawScale
    , _wallImpactEffectDrawScale   :: DrawScale

    , _hurtEffectData  :: EnemyHurtEffectData
    , _deathEffectData :: EnemyDeathEffectData
    , _spawnEffectData :: EnemySpawnEffectData
    }
    deriving Generic

instance FromJSON LankyEnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
