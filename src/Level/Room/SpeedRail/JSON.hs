module Level.Room.SpeedRail.JSON
    ( SpeedRailJSON(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Collision.Hitbox.Types
import Util

data SpeedRailJSON = SpeedRailJSON
    { _direction :: Direction
    , _hitbox    :: RectHitboxJSON
    }
    deriving Generic

instance FromJSON SpeedRailJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
