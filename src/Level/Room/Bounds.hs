module Level.Room.Bounds
    ( RoomArenaWallsBounds(..)
    , mkRoomArenaWallsBounds
    , RoomBounds(..)
    , mkRoomBounds
    ) where

import Data.Maybe (fromMaybe)

import Collision.Hitbox
import Level.Room.ArenaWalls.Types
import Util
import World.Surface.Types

data RoomArenaWallsBounds = RoomArenaWallsBounds
    { _innerLeftBounds  :: PosX
    , _innerRightBounds :: PosX
    , _topBounds        :: PosY
    }

mkRoomArenaWallsBounds :: RoomArenaWalls -> RoomArenaWallsBounds
mkRoomArenaWallsBounds arenaWalls = RoomArenaWallsBounds
    { _innerLeftBounds  = hitboxRight leftWallHbx
    , _innerRightBounds = hitboxLeft rightWallHbx
    , _topBounds        = min (hitboxTop leftWallHbx) (hitboxTop rightWallHbx)
    }
    where
        leftWallHbx  = _hitbox (_leftWall arenaWalls :: Surface)
        rightWallHbx = _hitbox (_rightWall arenaWalls :: Surface)

data RoomBounds = RoomBounds
    { _leftBounds          :: PosX
    , _rightBounds         :: PosX
    , _topBounds           :: PosY
    , _bottomBounds        :: PosY
    , _innerTopLeftBounds  :: PosX
    , _innerTopRightBounds :: PosX
    , _arenaWallsBounds    :: Maybe RoomArenaWallsBounds
    }

mkRoomBounds :: [Surface] -> RoomBounds
mkRoomBounds surfaces = RoomBounds
    { _leftBounds          = leftBounds
    , _rightBounds         = rightBounds
    , _topBounds           = topBounds
    , _bottomBounds        = maximumDefault $ map hitboxBot surfaceHbxs
    , _innerTopLeftBounds  = innerTopLeftBounds
    , _innerTopRightBounds = innerTopRightBounds
    , _arenaWallsBounds    = Nothing
    }
    where
        minimumDefault = \xs -> fromMaybe 0.0 (maybeMinimum xs)
        maximumDefault = \xs -> fromMaybe 0.0 (maybeMaximum xs)

        surfaceHbxs = map (_hitbox :: Surface -> Hitbox) surfaces
        leftBounds  = minimumDefault $ map hitboxLeft surfaceHbxs
        rightBounds = maximumDefault $ map hitboxRight surfaceHbxs
        topBounds   = minimumDefault $ map hitboxTop surfaceHbxs

        takeCloser :: Pos2 -> (Hitbox -> Pos2) -> Hitbox -> Hitbox -> Hitbox
        takeCloser pos hbxF hbx1 hbx2
            | hbx1DistSq <= hbx2DistSq = hbx1
            | otherwise                = hbx2
            where
                hbx1DistSq = vecDistSq pos (hbxF hbx1)
                hbx2DistSq = vecDistSq pos (hbxF hbx2)

        topLeftBounds      = Pos2 leftBounds topBounds
        innerTopLeftBounds = case surfaceHbxs of
            []     -> leftBounds
            (h:hs) -> hitboxRight $ foldr (takeCloser topLeftBounds hitboxTopLeft) h hs

        topRightBounds      = Pos2 rightBounds topBounds
        innerTopRightBounds = case surfaceHbxs of
            []     -> rightBounds
            (h:hs) -> hitboxLeft $ foldr (takeCloser topRightBounds hitboxTopRight) h hs
