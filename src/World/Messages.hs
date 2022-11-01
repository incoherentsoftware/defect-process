module World.Messages
    ( updateWorldMessages
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (foldlM)
import qualified Data.Map as M

import Level.Types
import Level.Room.Types
import Msg
import World
import World.Camera
import World.ScreenWipe
import World.Screenshake

updateWorldMessages :: (MonadIO m, MsgsRead UpdateWorldMsgsPhase m) => World -> m World
updateWorldMessages world = foldlM processMsg world =<< readMsgs
    where
        processMsg :: MonadIO m1 => World -> WorldMsgPayload -> m1 World
        processMsg !w d = case d of
            WorldMsgSwitchRoom roomName playerOffsetY -> do
                screenWipeOut <- mkWorldScreenWipeOut
                return $ w
                    { _screenWipe    = Just screenWipeOut
                    , _pendingChange = Just $ changeWorldRoom roomName playerOffsetY
                    }

            WorldMsgSaveRoomItems ->
                let
                    level             = _level w
                    room              = _room level
                    roomType          = _type (room :: Room)
                    roomItems         = _items room
                    roomItemsOverride = M.insert roomType roomItems (_roomItemsOverride level)
                in return $ w
                    { _level = level {_roomItemsOverride = roomItemsOverride}
                    }

            WorldMsgHitlag duration
                | duration > _hitlagTtl w -> return $ w {_hitlagTtl = duration}
                | otherwise               -> return w

            WorldMsgScreenshake magnitude -> return $ w {_screenshake = mkScreenshake magnitude}
            WorldMsgLockCamera            -> return $ w {_camera = setWorldCameraLocked (_camera w)}
            WorldMsgDeactivate            -> return $ w {_status = WorldDeadStatus}
            WorldMsgPause                 -> return w
