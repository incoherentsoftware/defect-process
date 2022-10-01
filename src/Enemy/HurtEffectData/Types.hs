module Enemy.HurtEffectData.Types
    ( EnemyHurtEffectData(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util
import Window.Graphics.Util

data EnemyHurtEffectData = EnemyHurtEffectData
    { _drawScale       :: DrawScale
    , _strongDrawScale :: DrawScale
    }
    deriving Generic

instance FromJSON EnemyHurtEffectData where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
