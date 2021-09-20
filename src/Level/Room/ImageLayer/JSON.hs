module Level.Room.ImageLayer.JSON
    ( RoomImageLayerJSON(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util

data RoomImageLayerJSON = RoomImageLayerJSON
    { _packFilePath  :: FilePath
    , _imageFileName :: FileName
    , _pos           :: Pos2
    , _zIndexOffset  :: Maybe Int
    , _parallax      :: Maybe Vec2
    , _rotated       :: Maybe Bool
    , _flipped       :: Maybe Bool
    }
    deriving Generic

instance FromJSON RoomImageLayerJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
