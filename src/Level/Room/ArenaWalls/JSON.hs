module Level.Room.ArenaWalls.JSON
    ( RoomArenaWallsJSON(..)
    , RoomArenaWallsGoldDropJSON(..)
    , RoomArenaWallsMaxWidthJSON(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Collision.Hitbox.Types
import Level.DangerValue
import Util
import World.Util

data RoomArenaWallsJSON = RoomArenaWallsJSON
    { _leftWall            :: RectHitboxJSON
    , _rightWall           :: RectHitboxJSON
    , _triggerHitbox       :: RectHitboxJSON
    , _enemySpawnPositions :: [Pos2]
    }
    deriving Generic

instance FromJSON RoomArenaWallsJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore

data RoomArenaWallsGoldDropJSON = RoomArenaWallsGoldDropJSON
    { _dangerValue :: DangerValue
    , _goldValue   :: GoldValue
    }
    deriving Generic

instance FromJSON RoomArenaWallsGoldDropJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore

data RoomArenaWallsMaxWidthJSON = RoomArenaWallsMaxWidthJSON
    { _maxDangerValue :: DangerValue
    , _maxWidth       :: Distance
    }
    deriving Generic

instance FromJSON RoomArenaWallsMaxWidthJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
