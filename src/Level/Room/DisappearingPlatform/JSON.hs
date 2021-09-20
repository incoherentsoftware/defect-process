module Level.Room.DisappearingPlatform.JSON
    ( DisappearingPlatformEntryJSON(..)
    , DisappearingPlatformJSON
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Collision.Hitbox.Types
import Util

data DisappearingPlatformEntryJSON = DisappearingPlatformEntryJSON
    { _durationSecs :: Secs
    , _hitbox       :: RectHitboxJSON
    }
    deriving Generic

instance FromJSON DisappearingPlatformEntryJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore

type DisappearingPlatformJSON = [DisappearingPlatformEntryJSON]
