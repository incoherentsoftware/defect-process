module World.Surface.Types
    ( WallSurfaceType(..)
    , SurfaceType(..)
    , Surface(..)
    ) where

import Collision.Hitbox.Types
import Util

data WallSurfaceType
    = LeftWallSurface
    | RightWallSurface
    deriving Eq

data SurfaceType
    = GeneralSurface
    | PlatformSurface
    | SpeedRailSurface Direction
    deriving Eq

data Surface = Surface
    { _type   :: SurfaceType
    , _hitbox :: Hitbox
    }
