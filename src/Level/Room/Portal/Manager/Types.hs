module Level.Room.Portal.Manager.Types
    ( RoomPortalManager(..)
    ) where

import Collision.Hitbox.Types
import Util
import Window.Graphics

data RoomPortalManager = RoomPortalManager
    { _hitbox             :: Hitbox
    , _infoPlayerX        :: Maybe PosX
    , _barrierPos         :: Pos2
    , _isBarrier          :: Bool
    , _barrierImage       :: Image
    , _barrierStrongImage :: Image
    }
