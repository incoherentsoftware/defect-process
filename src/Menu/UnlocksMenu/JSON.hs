module Menu.UnlocksMenu.JSON
    ( UnlocksEntryJSON(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util
import World.Util

data UnlocksEntryJSON a = UnlocksEntryJSON
    { _type          :: Maybe a
    , _cost          :: GoldValue
    , _imageFileName :: FileName
    , _pos           :: Pos2
    , _indices       :: (Int, Int, Int, Int, Int)
    }
    deriving Generic

instance FromJSON a => FromJSON (UnlocksEntryJSON a) where
    parseJSON value = genericParseJSON aesonFieldDropUnderscore value
