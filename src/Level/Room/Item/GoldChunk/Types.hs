module Level.Room.Item.GoldChunk.Types
    ( GoldChunkCount(..)
    ) where

import Data.Aeson.Types (FromJSON)

newtype GoldChunkCount = GoldChunkCount Int
    deriving newtype (FromJSON, Num)
