module Level.Room
    ( module Level.Room.Types
    , module Level.Room.Util
    , mkRoom
    , roomSurfaces
    , thinkRoom
    , updateRoom
    , drawRoom
    , preloadRoomDeferredPackResources
    ) where

import Control.Applicative ((<|>))
import Control.Monad       (when)
import Control.Monad.State (get, execStateT, lift, modify, put)
import Data.Dynamic        (toDyn)
import Data.Foldable       (foldlM, for_, sequenceA_, traverse_)
import Data.Traversable    (for)
import qualified Data.List as L

import AppEnv
import Async.Request
import Enemy.All
import Enemy.Types
import Level.Room.ArenaWalls
import Level.Room.Bounds
import Level.Room.Debug
import Level.Room.DisappearingPlatform
import Level.Room.DoorLightOverlay
import Level.Room.Empty
import Level.Room.ImageLayer
import Level.Room.Item as RI
import Level.Room.MovingPlatform
import Level.Room.Portal.Manager
import Level.Room.SpeedRail
import Level.Room.SpringLauncher
import Level.Room.Trigger
import Level.Room.Types
import Level.Room.Util
import Msg
import Util
import Window.Graphics
import World.Surface

levelItemsPackPath = "data/levels/level-items.pack" :: FilePath

mkRoom :: RoomType -> [Surface] -> Room
mkRoom roomType surfaces = mkEmptyRoom
    { _type     = roomType
    , _surfaces = surfaces
    , _bounds   = mkRoomBounds surfaces
    }

roomSurfaces :: Room -> [Surface]
roomSurfaces room = concat
    [ _surfaces room
    , arenaWallsSurfaces
    , speedRailSurfaces
    , disappearingPlatformSurfaces
    , movingPlatformSurfaces
    , springLauncherSurfaces
    , portalSurfaces
    ]
    where
        arenaWallsSurfaces           = maybe [] roomArenaWallsSurfaces (_arenaWalls room)
        speedRailSurfaces            = map speedRailSurface (_speedRails room)
        disappearingPlatformSurfaces = map disappearingPlatformSurface (_disappearingPlatforms room)
        movingPlatformSurfaces       = map movingPlatformSurface (_movingPlatforms room)
        springLauncherSurfaces       = map springLauncherSurface (_springLaunchers room)
        portalSurfaces               = maybe [] roomPortalManagerSurfaces (_portalManager room)

thinkRoom :: Room -> AppEnv ThinkLevelMsgsPhase ()
thinkRoom room = do
    let writeMsgsA = \m -> traverse_ writeMsgs m

    writeMsgsA =<< sequenceA [(_think (rt :: RoomTrigger)) room rt | rt <- _triggers room]
    writeMsgs =<< maybe (return []) thinkRoomArenaWalls (_arenaWalls room)
    writeMsgs $ concatMap thinkMovingPlatform (_movingPlatforms room)
    writeMsgs $ concatMap thinkSpringLauncher (_springLaunchers room)
    writeMsgsA =<< sequenceA [(RI._think ri) ri | Some ri <- _items room]

updateRoomBounds :: Room -> Room
updateRoomBounds room = room
    { _bounds = bounds {_arenaWallsBounds = arenaWallsBounds}
    }
    where
        bounds           = _bounds room
        arenaWallsBounds = case _arenaWalls room of
            Just arenaWalls
                | isRoomArenaWallsActive arenaWalls ->
                    _arenaWallsBounds bounds <|> Just (mkRoomArenaWallsBounds arenaWalls)
            _                                       -> Nothing

updateRoom :: Room -> AppEnv UpdateLevelMsgsPhase Room
updateRoom room = do
    triggers        <- traverse updateRoomTrigger (_triggers room)
    portalMgr       <- sequenceA $ updateRoomPortalManager <$> _portalManager room
    arenaWalls      <- sequenceA $ updateRoomArenaWalls <$> _arenaWalls room
    springLaunchers <- traverse updateSpringLauncher (_springLaunchers room)

    flip execStateT room $ do
        modify $ \r -> r
            { _triggers        = triggers
            , _portalManager   = portalMgr
            , _arenaWalls      = arenaWalls
            , _springLaunchers = springLaunchers
            }
        modify updateRoomBounds

        modify $ \r -> r
            { _speedRails            = map updateSpeedRail (_speedRails r)
            , _disappearingPlatforms = map updateDisappearingPlatform (_disappearingPlatforms r)
            , _movingPlatforms       = map updateMovingPlatform (_movingPlatforms r)
            }

        get >>= lift . updateRoomItems >>= put
        get >>= lift . processRoomMsgs >>= put

processRoomMsgs :: Room -> AppEnv UpdateLevelMsgsPhase Room
processRoomMsgs room = foldlM processMsg room =<< readMsgs
    where
        processMsg :: Room -> RoomMsgPayload -> AppEnv UpdateLevelMsgsPhase Room
        processMsg !r d = case d of
            RoomMsgAddTrigger trigger -> return $ r {_triggers = trigger:_triggers r}

            RoomMsgAddTriggerM mkTrigger -> do
                trigger <- mkTrigger
                return $ r {_triggers = trigger:_triggers r}

            RoomMsgRemoveTrigger triggerId -> return $ r
                { _triggers = [tr | tr <- _triggers r, _msgId (tr :: RoomTrigger) /= triggerId]
                }

            RoomMsgAddItemM mkItem -> do
                item <- mkItem
                return $ r {_items = item:_items r}

            RoomMsgRemoveItem itemId -> return $ r
                { _items = [Some i | Some i <- _items r, RI._msgId i /= itemId]
                }

            RoomMsgRemoveItemType itemType -> return $ r
                { _items = [Some i | Some i <- _items r, RI._type i /= itemType]
                }

            RoomMsgAddPortalBarrier -> return $ r
                { _portalManager = activateRoomPortalManagerBarrier <$> _portalManager r
                }

            RoomMsgKeepPortalBarrierAlive  -> return r
            RoomMsgReappearItem _          -> return r
            RoomMsgShowPickupItemIndicator -> return r
            RoomMsgUpdateArenaWalls _      -> return r
            RoomMsgUpdateTrigger _         -> return r
            RoomMsgUpdateItem _            -> return r
            RoomMsgUpdateSpringLauncher _  -> return r
            RoomMsgArenaWallsSplat _       -> return r

updateRoomTrigger :: MsgsRead UpdateLevelMsgsPhase m => RoomTrigger -> m RoomTrigger
updateRoomTrigger roomTrigger =
    let
        processMsg :: RoomTrigger -> RoomMsgPayload -> RoomTrigger
        processMsg rt d = case d of
            RoomMsgUpdateTrigger update -> update rt
            _                           -> rt
    in do
        msgs <- readMsgsTo $ _msgId (roomTrigger :: RoomTrigger)
        return $ L.foldl' processMsg roomTrigger msgs

updateRoomItems :: Room -> AppEnv UpdateLevelMsgsPhase Room
updateRoomItems room =
    let
        processMsg :: RoomItem d -> RoomMsgPayload -> RoomItem d
        processMsg item d = case d of
            RoomMsgUpdateItem update -> (RI._updateDynamic item) (toDyn update) item
            _                        -> item

        processMsgs :: RoomItem d -> AppEnv UpdateLevelMsgsPhase (RoomItem d)
        processMsgs item = L.foldl' processMsg item <$> readMsgsTo itemId
            where itemId = RI._msgId item
    in do
        items  <- for (_items room) $ \(Some item) -> Some <$> (RI._update item) item
        items' <- for items $ \(Some item) -> Some <$> processMsgs item
        return $ room {_items = items'}

drawRoom :: Room -> AppEnv DrawMsgsPhase ()
drawRoom room = do
    sequenceA_ $ drawRoomArenaWalls <$> _arenaWalls room
    sequenceA_ $ drawRoomPortalManager <$> _portalManager room

    traverse_ drawSpeedRail (_speedRails room)
    traverse_ drawDisappearingPlatform (_disappearingPlatforms room)
    traverse_ drawMovingPlatform (_movingPlatforms room)
    traverse_ drawSpringLauncher (_springLaunchers room)

    cameraPos <- getCameraPos
    traverse_ (drawRoomImageLayer cameraPos room) (_imageLayers room)
    sequenceA_ $ drawRoomDoorLightOverlay <$> _doorLightOverlay room

    for_ (_items room) $ \(Some ri) -> (RI._draw ri) ri

    drawRoomSurfaceHitboxesDebug room
    drawRoomPortalHitboxesDebug room

preloadRoomDeferredPackResources :: Room -> AppEnv p ()
preloadRoomDeferredPackResources room = do
    writeAsyncRequest $ PreloadPackFileRequest levelItemsPackPath
    sequenceA_ $ preloadRoomArenaWallsPackResources <$> _arenaWalls room

    when (_type (room :: Room) == endHallwayRoomType) $
        traverse_ (writeAsyncRequest . PreloadPackFileRequest) (enemyPreloadPackFilePathsFromType BossEnemy)
