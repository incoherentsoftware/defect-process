module Level.Room.MovingPlatform.Types
    ( MovingPlatform(..)
    ) where

import Util
import Window.Graphics

data MovingPlatform = MovingPlatform
    { _pos         :: Pos2
    , _dir         :: Direction
    , _width       :: Float
    , _height      :: Float
    , _leftBounds  :: PosX
    , _rightBounds :: PosX
    , _speed       :: Speed
    , _sprite      :: Sprite
    }
