module Level.Room.ArenaWalls.Ripple
    ( RoomArenaMarkerRipple
    , mkRoomArenaMarkerRipple
    , updateRoomArenaMarkerRipples
    , drawRoomArenaMarkerRipples
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (traverse_)

import Level.Room.ArenaWalls.Ripple.Types
import Level.Room.ArenaWalls.Types
import Level.Room.ArenaWalls.Util
import Util
import Window.Graphics
import World.ZIndex

disappearDecreaseOpacityRate = 0.08 :: Float

mkRoomArenaMarkerRipple :: RoomArenaWalls -> RoomArenaMarkerRipple
mkRoomArenaMarkerRipple arenaWalls = RoomArenaMarkerRipple
    { _pos     = roomArenaWallsMarkerPos arenaWalls
    , _opacity = Opacity 1.0
    , _sprite  = _markerRipple $ _sprites arenaWalls
    }

updateRoomArenaMarkerRipples :: RoomArenaWallsStatus -> [RoomArenaMarkerRipple] -> [RoomArenaMarkerRipple]
updateRoomArenaMarkerRipples arenaWallsStatus ripples = foldr updateRipple [] ripples
    where
        updateOpacity = case arenaWallsStatus of
            WallsReadyStatus -> id
            _                -> \o -> decreaseOpacity disappearDecreaseOpacityRate o

        updateRipple :: RoomArenaMarkerRipple -> [RoomArenaMarkerRipple] -> [RoomArenaMarkerRipple]
        updateRipple r !rs
            | isRemove  = rs
            | otherwise = r':rs
            where
                r' = r
                    { _opacity = updateOpacity $ _opacity r
                    , _sprite  = updateSprite $ _sprite r
                    }

                isRemove = spriteFinished (_sprite r') || isMinOpacity (_opacity r')

drawRoomArenaMarkerRipples :: forall m. (GraphicsReadWrite m, MonadIO m) => [RoomArenaMarkerRipple] -> m ()
drawRoomArenaMarkerRipples ripples = traverse_ drawRipple ripples
    where
        drawRipple :: RoomArenaMarkerRipple -> m ()
        drawRipple ripple = drawSpriteEx pos RightDir levelArenaWallsZIndex 0.0 opacity NonScaled spr
            where
                pos     = _pos ripple
                opacity = _opacity ripple
                spr     = _sprite ripple
