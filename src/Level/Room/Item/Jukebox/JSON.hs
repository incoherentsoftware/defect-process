module Level.Room.Item.Jukebox.JSON
    ( JukeboxJSON(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Level.Room.Item.Jukebox.Types
import Util

data JukeboxJSON = JukeboxJSON
    { _pos  :: Pos2
    , _type :: JukeboxType
    }
    deriving Generic

instance FromJSON JukeboxJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
