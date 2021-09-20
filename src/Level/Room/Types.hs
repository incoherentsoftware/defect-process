module Level.Room.Types
    ( module Level.Room.Bounds
    , RoomName
    , RoomType(..)
    , Room(..)
    ) where

import qualified Data.Text as T

import Level.Room.ArenaWalls.Types
import Level.Room.Bounds
import Level.Room.DisappearingPlatform
import Level.Room.DoorLightOverlay
import Level.Room.ImageLayer.Types
import Level.Room.Item.Types
import Level.Room.MovingPlatform.Types
import Level.Room.Portal.Manager.Types
import Level.Room.SpeedRail
import Level.Room.SpringLauncher.Types
import Level.Room.Trigger.Types
import Util
import World.Surface.Types

type RoomName = T.Text

data RoomType
    = EmptyRoomType
    | NextRoomType
    | ArenaRoomType RoomName
    | FromTransitionRoomType RoomName
    | ToTransitionRoomType RoomName
    | ChallengeRoomType RoomName
    | SpecialRoomType RoomName
    deriving (Eq, Show)
    deriving anyclass PrettyShow

data Room = Room
    { _type                  :: RoomType
    , _surfaces              :: [Surface]
    , _playerSpawnPos        :: Pos2
    , _items                 :: [Some RoomItem]
    , _portalManager         :: Maybe RoomPortalManager
    , _doorLightOverlay      :: Maybe RoomDoorLightOverlay
    , _arenaWalls            :: Maybe RoomArenaWalls
    , _speedRails            :: [SpeedRail]
    , _disappearingPlatforms :: [DisappearingPlatform]
    , _movingPlatforms       :: [MovingPlatform]
    , _springLaunchers       :: [SpringLauncher]
    , _imageLayers           :: [RoomImageLayer]
    , _bounds                :: RoomBounds
    , _minCameraY            :: PosY
    , _cameraBottomLocked    :: Bool
    , _cameraPlayerOffsetY   :: PosY
    , _triggers              :: [RoomTrigger]
    }
