module Level.Room.Portal.JSON
    ( PortalJSON(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Collision.Hitbox.Types
import Util

data PortalJSON = PortalJSON
    { _bounds     :: RectHitboxJSON
    , _barrierPos :: Maybe Pos2
    , _isBarrier  :: Maybe Bool
    }
    deriving Generic

instance FromJSON PortalJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
