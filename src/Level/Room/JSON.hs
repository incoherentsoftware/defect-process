module Level.Room.JSON
    ( RoomJSON(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Collision.Hitbox.Types
import Level.Room.ArenaWalls.JSON
import Level.Room.DisappearingPlatform.JSON
import Level.Room.DoorLightOverlay
import Level.Room.ImageLayer.JSON
import Level.Room.Item.EventActivator.JSON
import Level.Room.Item.GoldChunk.JSON
import Level.Room.Item.Jukebox.JSON
import Level.Room.Item.Pickup.JSON
import Level.Room.MovingPlatform.JSON
import Level.Room.Portal.JSON
import Level.Room.SpeedRail.JSON
import Level.Room.SpringLauncher.JSON
import Util

data RoomJSON = RoomJSON
    { _playerSpawn           :: Pos2
    , _surfaces              :: [RectHitboxJSON]
    , _imageLayers           :: [RoomImageLayerJSON]
    , _portal                :: Maybe PortalJSON
    , _doorLightOverlay      :: Maybe RoomDoorLightOverlayJSON
    , _platforms             :: Maybe [RectHitboxJSON]
    , _items                 :: Maybe [RoomItemPickupJSON]
    , _arenaWalls            :: Maybe RoomArenaWallsJSON
    , _goldChunks            :: Maybe [GoldChunkSetJSON]
    , _infoSigns             :: Maybe [Pos2]
    , _refreshStations       :: Maybe [Pos2]
    , _jukeboxes             :: Maybe [JukeboxJSON]
    , _eventActivator        :: Maybe EventActivatorJSON
    , _speedRails            :: Maybe [SpeedRailJSON]
    , _disappearingPlatforms :: Maybe [DisappearingPlatformJSON]
    , _movingPlatforms       :: Maybe [MovingPlatformJSON]
    , _springLaunchers       :: Maybe [SpringLauncherJSON]
    , _minCameraY            :: Maybe PosY
    , _cameraBottomLocked    :: Maybe Bool
    , _cameraPlayerOffsetY   :: Maybe PosY
    }
    deriving Generic

instance FromJSON RoomJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
