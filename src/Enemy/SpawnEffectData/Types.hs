module Enemy.SpawnEffectData.Types
    ( EnemySpawnEffectData(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util
import Window.Graphics.Util

data EnemySpawnEffectData = EnemySpawnEffectData
    { _drawScale :: DrawScale
    , _offset    :: Maybe Pos2
    , _inAir     :: Maybe Bool
    }
    deriving Generic

instance FromJSON EnemySpawnEffectData where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
