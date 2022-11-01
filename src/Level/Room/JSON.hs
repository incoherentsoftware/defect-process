module Level.Room.JSON
    ( SignTypeJSON(..)
    , SignJSON(..)
    , RoomRandomOffsetsJSON(..)
    , RoomJSON(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T

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

data SignTypeJSON
    = InfoSignType
    | TutorialSignType
    deriving (FromJSON, Generic)

data SignJSON = SignJSON
    { _type :: SignTypeJSON
    , _pos  :: Pos2
    }
    deriving Generic

instance FromJSON SignJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore

data RoomRandomOffsetsJSON = RoomRandomOffsetsJSON
    { _platforms   :: Maybe (M.Map Int T.Text)
    , _imageLayers :: Maybe (M.Map Int T.Text)
    , _goldChunks  :: Maybe (M.Map Int T.Text)
    , _vars        :: M.Map T.Text (NE.NonEmpty Pos2)
    }
    deriving Generic

instance FromJSON RoomRandomOffsetsJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore

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
    , _signs                 :: Maybe [SignJSON]
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
    , _randomOffsets         :: Maybe RoomRandomOffsetsJSON
    }
    deriving Generic

instance FromJSON RoomJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
