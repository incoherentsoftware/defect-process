module World.Camera
    ( WorldCamera
    , mkWorldCamera
    , calculateWorldCameraPos
    , updateWorldCamera
    , setWorldCameraLocked
    , drawWorldCamera
    ) where

import Control.Monad          (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)

import Configs
import Configs.All.Player
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Level.Room.ArenaWalls
import Level.Room.Types
import Player.Types
import Player.Util
import Util
import Window.Graphics
import World.Camera.Types

cameraWidth      = virtualRenderWidth        :: Float
cameraHeight     = virtualRenderHeight       :: Float
cameraHalfWidth  = virtualRenderWidth / 2.0  :: Float
cameraHalfHeight = virtualRenderHeight / 2.0 :: Float

cameraSetOverrideSpeedX   = 1000.0 :: SpeedX
cameraUnsetOverrideSpeedX = 600.0  :: SpeedX
arenaWallsCameraDistance  = 100.0  :: Distance

mkWorldCamera :: WorldCamera
mkWorldCamera = WorldCamera
    { _locked              = False
    , _leftBoundsOverride  = Nothing
    , _rightBoundsOverride = Nothing
    }

calculateWorldCameraPos :: Player -> Room -> Pos2 -> Pos2 -> WorldCamera -> Pos2
calculateWorldCameraPos player room lerpOffset debugOffset worldCamera = Pos2 cameraX cameraY `vecAdd` debugOffset
    where
        Pos2 playerX playerY = _pos (player :: Player) `vecAdd` lerpOffset
        playerY'             = playerY - _height (_config player :: PlayerConfig) / 2.0 + _cameraPlayerOffsetY room
        bounds               = _bounds room

        leftBounds  = fromMaybe (_leftBounds bounds) (_leftBoundsOverride worldCamera)
        rightBounds = fromMaybe (_rightBounds bounds) (_rightBoundsOverride worldCamera)

        bottomBounds    = _bottomBounds bounds
        boundsWidth     = rightBounds - leftBounds
        minCameraY      = _minCameraY room
        cameraBotLocked = _cameraBottomLocked room

        cameraX
            | boundsWidth < cameraWidth               = leftBounds - (cameraWidth - boundsWidth) / 2.0
            | playerX - leftBounds < cameraHalfWidth  = leftBounds
            | playerX + cameraHalfWidth > rightBounds = rightBounds - cameraWidth
            | otherwise                               = playerX - cameraHalfWidth

        cameraY
            | cameraBotLocked || bottomBounds - playerY' < cameraHalfHeight = bottomBounds - cameraHeight
            | playerY' - cameraHalfHeight < minCameraY                      = minCameraY
            | otherwise                                                     = playerY' - cameraHalfHeight

calculateArenaWallsCameraBounds :: RoomArenaWalls -> (PosX, PosX)
calculateArenaWallsCameraBounds arenaWalls = (left', right')
    where
        left  = vecX $ roomArenaWallsLeftWallPos arenaWalls
        right = vecX $ roomArenaWallsRightWallPos arenaWalls
        diff  = right - left

        (left', right')
            | diff < cameraWidth =
                let offset = (cameraWidth - diff) / 2.0
                in (left - offset, right + offset)
            | otherwise          = (left - arenaWallsCameraDistance, right + arenaWallsCameraDistance)

updateWorldCamera :: Player -> Room -> WorldCamera -> WorldCamera
updateWorldCamera player room worldCamera = worldCamera
    { _locked              = False
    , _leftBoundsOverride  = leftBoundsOverride
    , _rightBoundsOverride = rightBoundsOverride
    }
    where
        cameraLeft  = vecX $ calculateWorldCameraPos player room zeroPos2 zeroPos2 worldCamera
        cameraRight = cameraLeft + cameraWidth

        (leftBoundsOverride, rightBoundsOverride) = case _arenaWalls room of
            Just arenaWalls
                | isRoomArenaWallsActive arenaWalls ->
                    let
                        (arenaWallsLeft, arenaWallsRight) = calculateArenaWallsCameraBounds arenaWalls

                        leftOverride
                            | cameraLeft >= arenaWallsLeft = arenaWallsLeft
                            | otherwise                    =
                                min arenaWallsLeft (cameraLeft + cameraSetOverrideSpeedX * timeStep)

                        rightOverride
                            | cameraRight <= arenaWallsRight = arenaWallsRight
                            | otherwise                      =
                                max arenaWallsRight (cameraRight - cameraSetOverrideSpeedX * timeStep)
                    in (Just leftOverride, Just rightOverride)

            _ ->
                let
                    bounds          = _bounds room
                    roomLeftBounds  = _leftBounds bounds
                    roomRightBounds = _rightBounds bounds
                    playerX         = vecX $ _pos (player :: Player)

                    leftOverride = case _leftBoundsOverride worldCamera of
                        Just left
                            | playerX - cameraHalfWidth >= left -> Nothing
                            | left > roomLeftBounds             ->
                                Just $ max roomLeftBounds (left - cameraUnsetOverrideSpeedX * timeStep)
                        _                                       -> Nothing

                    rightOverride = case _rightBoundsOverride worldCamera of
                        Just right
                            | playerX + cameraHalfWidth <= right -> Nothing
                            | right < roomRightBounds            ->
                                Just $ min roomRightBounds (right + cameraUnsetOverrideSpeedX * timeStep)
                        _                                        -> Nothing
                in (leftOverride, rightOverride)

setWorldCameraLocked :: WorldCamera -> WorldCamera
setWorldCameraLocked worldCamera = worldCamera {_locked = True}

drawWorldCamera :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => Player -> Room -> WorldCamera -> m ()
drawWorldCamera player room worldCamera = unless (_locked worldCamera) $ do
    lerpOffset  <- playerLerpOffset player
    debugOffset <- readSettingsConfig _debug _cameraDebugOffset
    setCameraPos $ calculateWorldCameraPos player room lerpOffset debugOffset worldCamera
