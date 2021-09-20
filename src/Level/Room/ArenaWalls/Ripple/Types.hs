module Level.Room.ArenaWalls.Ripple.Types
    ( RoomArenaMarkerRipple(..)
    ) where

import Util
import Window.Graphics

data RoomArenaMarkerRipple = RoomArenaMarkerRipple
    { _pos     :: Pos2
    , _opacity :: Opacity
    , _sprite  :: Sprite
    }
