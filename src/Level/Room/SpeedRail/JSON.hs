module Level.Room.SpeedRail.JSON
    ( SpeedRailJSON(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util

data SpeedRailJSON = SpeedRailJSON
    { _pos         :: Pos2
    , _direction   :: Direction
    , _numSegments :: Int
    }
    deriving Generic

instance FromJSON SpeedRailJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
