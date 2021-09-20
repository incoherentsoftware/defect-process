module Level.Room.Empty
    ( mkEmptyRoom
    ) where

import Level.Room.Bounds
import Level.Room.Types
import Level.Room.Util
import Util

mkEmptyRoom :: Room
mkEmptyRoom = Room
    { _type                  = EmptyRoomType
    , _surfaces              = []
    , _playerSpawnPos        = zeroPos2
    , _items                 = []
    , _portalManager         = Nothing
    , _doorLightOverlay      = Nothing
    , _arenaWalls            = Nothing
    , _speedRails            = []
    , _disappearingPlatforms = []
    , _movingPlatforms       = []
    , _springLaunchers       = []
    , _imageLayers           = []
    , _bounds                = mkRoomBounds []
    , _minCameraY            = defaultRoomMinCameraY
    , _cameraBottomLocked    = False
    , _cameraPlayerOffsetY   = 0.0
    , _triggers              = []
    }
