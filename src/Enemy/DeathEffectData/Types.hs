module Enemy.DeathEffectData.Types
    ( EnemyDeathEffectData(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util
import Window.Graphics.Util

data EnemyDeathEffectData = EnemyDeathEffectData
    { _drawScale :: DrawScale
    , _offset    :: Maybe Pos2
    }
    deriving Generic

instance FromJSON EnemyDeathEffectData where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
