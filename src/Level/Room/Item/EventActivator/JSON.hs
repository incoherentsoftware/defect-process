module Level.Room.Item.EventActivator.JSON
    ( EventActivatorJSON(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Level.Room.Event.Types
import Util

data EventActivatorJSON = EventActivatorJSON
    { _pos  :: Pos2
    , _type :: RoomEventType
    }
    deriving Generic

instance FromJSON EventActivatorJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
