module Level.Room.Item.GoldChunk.JSON
    ( GoldChunkJSON(..)
    , GoldChunkSetJSON(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Level.Room.Item.GoldChunk.Types
import Util

data GoldChunkJSON = GoldChunkJSON
    { _pos   :: Pos2
    , _count :: GoldChunkCount
    }
    deriving Generic

instance FromJSON GoldChunkJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore

data GoldChunkSetJSON = GoldChunkSetJSON
    { _chunks           :: [GoldChunkJSON]
    , _chanceMultiplier :: Maybe Int
    }
    deriving Generic

instance FromJSON GoldChunkSetJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
