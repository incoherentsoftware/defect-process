module Level.Room.ArenaWalls.EnemySpawn.Types
    ( EnemySpawnWaveChoiceRule(..)
    , EnemySpawnWaveChoice(..)
    , EnemySpawnWave(..)
    , EnemySpawnWaveJSON(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)
import qualified Data.Text as T

import {-# SOURCE #-} Enemy.Types
import Level.DangerValue
import Util

data EnemySpawnWaveChoiceRule
    = MaxOneAnyTurretRule
    | MaxOneBombRule
    | MaxOneBubbleTurretRule
    | MaxOneFlyingRule
    | MaxTwoFlyingRule
    | MaxOneHopRule
    | MaxOneSpearRule
    | MaxOneTurretRule
    | MaxOneWallRule
    deriving (Eq, FromJSON, Generic)

data EnemySpawnWaveChoice = EnemySpawnWaveChoice
    { _chanceMultiplier :: Maybe Int
    , _parseTexts       :: [T.Text]
    , _rules            :: Maybe [EnemySpawnWaveChoiceRule]
    }
    deriving (Eq, Generic)

instance FromJSON EnemySpawnWaveChoice where
    parseJSON = genericParseJSON aesonFieldDropUnderscore

data EnemySpawnWave = EnemySpawnWave
    { _dangerValue :: DangerValue
    , _enemyTypes  :: [EnemyType]
    }
    deriving Eq

data EnemySpawnWaveJSON = EnemySpawnWaveJSON
    { _dangerValue  :: DangerValue
    , _numWaves     :: Int
    , _enemyChoices :: [EnemySpawnWaveChoice]
    }
    deriving (Eq, Generic)

instance FromJSON EnemySpawnWaveJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
