module Level.Room.ImageLayer.Types
    ( RoomImageLayer(..)
    ) where

import Util
import Window.Graphics

data RoomImageLayer = RoomImageLayer
    { _image    :: Image
    , _pos      :: Pos2
    , _zIndex   :: ZIndex
    , _parallax :: Vec2
    , _rotated  :: Bool
    , _dir      :: Direction
    }
