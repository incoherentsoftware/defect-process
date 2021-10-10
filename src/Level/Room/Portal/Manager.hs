module Level.Room.Portal.Manager
    ( RoomPortalManager(_isBarrier)
    , mkRoomPortalManager
    , updateRoomPortalManager
    , drawRoomPortalManager
    , activateRoomPortalManagerBarrier
    , isRoomPortalManagerBarrierPlayerClose
    , roomPortalManagerHitboxes
    , roomPortalManagerSurfaces
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)

import Collision.Hitbox
import Constants
import FileCache
import InfoMsg.Util
import Level.Room.Portal.JSON
import Level.Room.Portal.Manager.Types
import Msg
import Util
import Window.Graphics
import World.Surface
import World.ZIndex

barrierStrongDistanceX = 100.0 :: Distance
barrierTimeoutSecs     = 0.5   :: Secs

packPath               = \f -> PackResourceFilePath "data/levels/level-items.pack" f
barrierImagePath       = packPath "exit-barrier.image"        :: PackResourceFilePath
barrierStrongImagePath = packPath "exit-barrier-strong.image" :: PackResourceFilePath

mkRoomPortalManager :: (FileCache m, GraphicsRead m, MonadIO m) => PortalJSON -> m RoomPortalManager
mkRoomPortalManager json = do
    barrierImg       <- loadPackImage barrierImagePath
    barrierStrongImg <- loadPackImage barrierStrongImagePath

    let
        hbx        = _fromJSON $ _bounds json
        isBarrier  = fromMaybe False (_isBarrier (json :: PortalJSON))
        barrierTtl = if isBarrier then barrierTimeoutSecs else 0.0

    return $ RoomPortalManager
        { _hitbox             = hbx
        , _infoPlayerX        = Nothing
        , _barrierPos         = fromMaybe (hitboxBotLeft hbx) (_barrierPos (json :: PortalJSON))
        , _isBarrier          = isBarrier
        , _barrierTtl         = barrierTtl
        , _barrierImage       = barrierImg
        , _barrierStrongImage = barrierStrongImg
        }

updateRoomPortalManager :: MsgsRead UpdateLevelMsgsPhase m => RoomPortalManager -> m RoomPortalManager
updateRoomPortalManager portalMgr =
    let
        processInfoMsgPlayer :: [InfoMsgPayload] -> Maybe PosX
        processInfoMsgPlayer []     = Nothing
        processInfoMsgPlayer (p:ps) = case p of
            InfoMsgPlayer playerInfo -> Just $ vecX (playerInfoPos playerInfo)
            _                        -> processInfoMsgPlayer ps

        processRoomMsgKeepPortalBarrierAlive :: [RoomMsgPayload] -> Bool
        processRoomMsgKeepPortalBarrierAlive []     = False
        processRoomMsgKeepPortalBarrierAlive (p:ps) = case p of
            RoomMsgKeepPortalBarrierAlive -> True
            _                             -> processRoomMsgKeepPortalBarrierAlive ps
    in do
        infoPlayerX        <- processInfoMsgPlayer <$> readMsgs
        isKeepBarrierAlive <- processRoomMsgKeepPortalBarrierAlive <$> readMsgs

        let
            barrierTtl                                = _barrierTtl portalMgr
            isBarrier                                 = _isBarrier (portalMgr :: RoomPortalManager)
            barrierTtl'
                | isBarrier && not isKeepBarrierAlive = barrierTtl - timeStep
                | otherwise                           = barrierTtl
            isBarrier'
                | isBarrier && barrierTtl' <= 0.0     = False
                | otherwise                           = isBarrier

        return $ portalMgr
            { _infoPlayerX = infoPlayerX
            , _isBarrier   = isBarrier'
            , _barrierTtl  = barrierTtl'
            }

drawRoomPortalManager :: (FileCache m, GraphicsReadWrite m, MonadIO m) => RoomPortalManager -> m ()
drawRoomPortalManager portalMgr = when (_isBarrier (portalMgr :: RoomPortalManager)) $
    let
        barrierPos = _barrierPos (portalMgr :: RoomPortalManager)
        img        = if
            | isRoomPortalManagerBarrierPlayerClose portalMgr -> _barrierStrongImage portalMgr
            | otherwise                                       -> _barrierImage portalMgr
    in drawImage barrierPos RightDir levelArenaWallsZIndex img

activateRoomPortalManagerBarrier :: RoomPortalManager -> RoomPortalManager
activateRoomPortalManagerBarrier portalMgr = portalMgr
    { _isBarrier  = True
    , _barrierTtl = barrierTimeoutSecs
    }

roomPortalManagerHitboxes :: RoomPortalManager -> [Hitbox]
roomPortalManagerHitboxes portalMgr
    | _isBarrier (portalMgr :: RoomPortalManager) = []
    | otherwise                                   = [_hitbox (portalMgr :: RoomPortalManager)]

roomPortalManagerSurfaces :: RoomPortalManager -> [Surface]
roomPortalManagerSurfaces portalMgr
    | _isBarrier (portalMgr :: RoomPortalManager) = [mkGeneralSurface $ _hitbox (portalMgr :: RoomPortalManager)]
    | otherwise                                   = []

isRoomPortalManagerBarrierPlayerClose :: RoomPortalManager -> Bool
isRoomPortalManagerBarrierPlayerClose portalMgr
    | _isBarrier (portalMgr :: RoomPortalManager) =
        let barrierPos = _barrierPos (portalMgr :: RoomPortalManager)
        in case _infoPlayerX portalMgr of
            Just playerX -> abs (playerX - vecX barrierPos) <= barrierStrongDistanceX
            Nothing      -> False
    | otherwise                                   = False
