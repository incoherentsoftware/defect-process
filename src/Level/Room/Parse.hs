module Level.Room.Parse
    ( loadRoomItemsShop
    , loadRoom
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import Data.Traversable       (for)
import Data.Yaml              (decodeEither')
import System.FilePath        (takeDirectory)
import qualified Data.Map as M
import qualified Data.Set as S

import AppEnv
import Attack.Util
import Collision
import Configs
import Configs.All.Progress
import FileCache
import Level.DangerValue
import Level.Room
import Level.Room.ArenaWalls
import Level.Room.Chooser
import Level.Room.DisappearingPlatform
import Level.Room.DoorLightOverlay
import Level.Room.Empty
import Level.Room.ImageLayer
import Level.Room.Item.EventActivator
import Level.Room.Item.EventActivator.JSON
import Level.Room.Item.GoldChunk
import Level.Room.Item.InfoSign
import Level.Room.Item.Jukebox
import Level.Room.Item.Jukebox.JSON
import Level.Room.Item.Pickup.All
import Level.Room.Item.RefreshStation
import Level.Room.Item.TutorialSign
import Level.Room.Item.Types as RI
import Level.Room.JSON
import Level.Room.MovingPlatform
import Level.Room.Portal.Manager
import Level.Room.SpeedRail
import Level.Room.SpringLauncher
import Level.Room.SpringLauncher.JSON
import Level.Room.Trigger.All
import Player.EquipmentInfo
import Player.RoomInfo
import Stats.Manager
import Util
import Window.Graphics
import Window.InputState
import World.Audio.LayeredMusic
import World.Surface

loadRoomRefreshStations :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m) => RoomJSON -> m [Some RoomItem]
loadRoomRefreshStations roomJSON = traverse mkRefreshStation positions
    where positions = fromMaybe [] (_refreshStations roomJSON)

loadRoomItemsShop :: RoomJSON -> RoomType -> PlayerRoomInfo -> StatsManager -> AppEnv p [Some RoomItem]
loadRoomItemsShop roomJSON roomType playerRoomInfo statsManager = do
    itemPickups     <- chooseLoadRoomItemPickups roomJSON roomType playerRoomInfo statsManager
    refreshStations <- if
        | null itemPickups -> return []
        | otherwise        -> loadRoomRefreshStations roomJSON
    return $ refreshStations ++ itemPickups

loadRoomJukeboxes
    :: forall m. (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => [JukeboxJSON]
    -> m [Some RoomItem]
loadRoomJukeboxes []     = return []
loadRoomJukeboxes (j:js) = do
    unlockedMusic <- readConfig _progress _unlockedMusic
    case _type (j :: JukeboxJSON) of
        BattleJukeboxType
            | S.size (S.filter isBattleMusicType unlockedMusic) > 1  -> (:) <$> mkJukebox j <*> loadRoomJukeboxes js
        ExplorationJukeboxType
            | S.size (S.filter isExploreMusicType unlockedMusic) > 1 -> (:) <$> mkJukebox j <*> loadRoomJukeboxes js
        _                                                            -> loadRoomJukeboxes js

loadRoom :: RoomType -> PlayerRoomInfo -> RoomChooser -> DangerValue -> StatsManager -> AppEnv p Room
loadRoom EmptyRoomType _ _ _ _                                               = return mkEmptyRoom
loadRoom roomType playerRoomInfo roomChooser currentDangerValue statsManager = do
    let
        filePath = case roomType of
            NextRoomType -> roomTypeToFilePath $ nextRoomChooserRoomType roomChooser
            _            -> roomTypeToFilePath roomType

    roomByteStr <- readFileCache filePath
    case decodeEither' roomByteStr of
        Left e         -> error $ filePath ++ ": " ++ show e
        Right roomJSON ->
            let
                surfaces            = _surfaces (roomJSON :: RoomJSON)
                surfaces'           = map (mkGeneralSurface . _fromJSON) surfaces
                platforms           = fromMaybe [] (_platforms (roomJSON :: RoomJSON))
                platforms'          = map (mkPlatformSurface . _fromJSON) platforms
                allSurfaces         = surfaces' ++ platforms'
                playerSpawn         = _playerSpawn roomJSON
                filePathDir         = takeDirectory filePath
                minCameraY          = fromMaybe defaultRoomMinCameraY (_minCameraY (roomJSON :: RoomJSON))
                cameraBotLocked     = fromMaybe False (_cameraBottomLocked (roomJSON :: RoomJSON))
                cameraPlayerOffsetY = fromMaybe 0.0 (_cameraPlayerOffsetY (roomJSON :: RoomJSON))
                newRoomType         = roomFilePathToType filePath
            in do
                goldChunks <- case _goldChunks (roomJSON :: RoomJSON) of
                    Just cs -> mkGoldChunkSet cs
                    Nothing -> return []

                signs <- case _signs roomJSON of
                    Just ss -> for ss $ \s -> case _type (s :: SignJSON) of
                        InfoSignType     -> mkInfoSign $ _pos (s :: SignJSON)
                        TutorialSignType -> mkTutorialSign $ _pos (s :: SignJSON)
                    Nothing -> return []

                jukeboxes <- maybe (return []) loadRoomJukeboxes (_jukeboxes roomJSON)

                eventActivators <- case _eventActivator (roomJSON :: RoomJSON) of
                    Nothing -> return []
                    Just ea ->
                        let
                            eaType = _type (ea :: EventActivatorJSON)
                            eaPos  = _pos (ea :: EventActivatorJSON)
                        in pure <$> mkEventActivator eaType eaPos

                otherItems <- case roomChooserRoomContentType newRoomType roomChooser of
                    ShopContentType ->
                        let
                            playerRoomInfo' = playerRoomInfo
                                { _equipment = if
                                    | roomType == startingShopRoomType -> mkEmptyPlayerEquipmentInfo
                                    | otherwise                        -> _equipment playerRoomInfo
                                }
                        in loadRoomItemsShop roomJSON newRoomType playerRoomInfo' statsManager

                    HealthContentType
                        | isHealthMax (_health playerRoomInfo) -> loadRoomItemGoldChunk roomJSON
                        | otherwise                            ->
                            loadRoomItemHealthPickups roomJSON newRoomType statsManager

                    GoldChunkContentType       -> loadRoomItemGoldChunk roomJSON
                    EventContentType eventType -> loadRoomItemEvent roomJSON eventType

                    NoContentType -> if
                        | isTransitionRoomType newRoomType -> return []
                        | otherwise                        -> loadRoomRefreshStations roomJSON

                let roomItems = goldChunks ++ signs ++ jukeboxes ++ eventActivators ++ otherItems

                portalMgr <- case _portal (roomJSON :: RoomJSON) of
                    Nothing -> return Nothing
                    Just p  -> Just <$> mkRoomPortalManager p

                arenaWalls <- case _arenaWalls (roomJSON :: RoomJSON) of
                    Just w  -> Just <$> mkRoomArenaWalls currentDangerValue w
                    Nothing -> return Nothing

                speedRails <- case _speedRails (roomJSON :: RoomJSON) of
                    Just rs -> traverse mkSpeedRail rs
                    Nothing -> return []

                disappearingPlatforms <- case _disappearingPlatforms (roomJSON :: RoomJSON) of
                    Just ps -> traverse mkDisappearingPlatform ps
                    Nothing -> return []

                movingPlatforms <- case _movingPlatforms (roomJSON :: RoomJSON) of
                    Just ps -> traverse mkMovingPlatform ps
                    Nothing -> return []

                springLaunchers <- case _springLaunchers (roomJSON :: RoomJSON) of
                    Just ss -> traverse mkSpringLauncher [_pos (s :: SpringLauncherJSON) | s <- ss]
                    Nothing -> return []

                imageLayers <- traverse (loadRoomImageLayer filePathDir) (_imageLayers (roomJSON :: RoomJSON))

                doorLightOverlay <- case _doorLightOverlay (roomJSON :: RoomJSON) of
                    Nothing          -> return Nothing
                    Just doorOverlay ->
                        let
                            pos    = _pos (doorOverlay :: RoomDoorLightOverlayJSON)
                            height = _height (doorOverlay :: RoomDoorLightOverlayJSON)
                        in Just <$> mkRoomDoorLightOverlay pos height

                triggers <- mkRoomTriggers newRoomType

                let
                    newRoom = (mkRoom newRoomType allSurfaces)
                        { _playerSpawnPos        = playerSpawn
                        , _items                 = roomItems
                        , _portalManager         = portalMgr
                        , _doorLightOverlay      = doorLightOverlay
                        , _arenaWalls            = arenaWalls
                        , _speedRails            = speedRails
                        , _disappearingPlatforms = disappearingPlatforms
                        , _movingPlatforms       = movingPlatforms
                        , _springLaunchers       = springLaunchers
                        , _imageLayers           = imageLayers
                        , _triggers              = triggers
                        , _minCameraY            = minCameraY
                        , _cameraBottomLocked    = cameraBotLocked
                        , _cameraPlayerOffsetY   = cameraPlayerOffsetY
                        }
                newRoom' <- applyRandomOffsets (_randomOffsets roomJSON) newRoom

                preloadRoomDeferredPackResources newRoom'
                return newRoom'

applyRandomOffsets :: MonadIO m => Maybe RoomRandomOffsetsJSON -> Room -> m Room
applyRandomOffsets Nothing room                  = return room
applyRandomOffsets (Just randomOffsetsJSON) room = do
    vars <- M.fromList <$> sequenceA
        [ (name,) <$> randomChoice values
        | (name, values) <- M.toList $ _vars randomOffsetsJSON
        ]

    let
        nonPlatforms = [s | s <- _surfaces (room :: Room), _type (s :: Surface) /= PlatformSurface]
        platforms    = [s | s <- _surfaces (room :: Room), _type (s :: Surface) == PlatformSurface]
        platforms'   = case _platforms (randomOffsetsJSON :: RoomRandomOffsetsJSON) of
            Nothing              -> platforms
            Just platformOffsets ->
                [ case i `M.lookup` platformOffsets >>= (`M.lookup` vars) of
                    Nothing     -> platform
                    Just offset ->
                        let hbx = _hitbox (platform :: Surface)
                        in platform {_hitbox = moveHitbox offset hbx} :: Surface
                | (i, platform) <- zip [0..] platforms
                ]

        nonGoldChunks = [Some ri | Some ri <- _items (room :: Room), RI._type ri /= GoldChunkItemType]
        goldChunks    = [Some ri | Some ri <- _items (room :: Room), RI._type ri == GoldChunkItemType]
        goldChunks'   = case _goldChunks (randomOffsetsJSON :: RoomRandomOffsetsJSON) of
            Nothing                -> goldChunks
            Just goldChunkOffsets ->
                [ case i `M.lookup` goldChunkOffsets >>= (`M.lookup` vars) of
                    Nothing     -> Some goldChunk
                    Just offset -> Some $ goldChunk {RI._hitbox = moveHitbox offset (RI._hitbox goldChunk)}
                | (i, Some goldChunk) <- zip [0..] goldChunks
                ]

        imageLayers  = _imageLayers (room :: Room)
        imageLayers' = case _imageLayers (randomOffsetsJSON :: RoomRandomOffsetsJSON) of
            Nothing                -> imageLayers
            Just imageLayerOffsets ->
                [ case i `M.lookup` imageLayerOffsets >>= (`M.lookup` vars) of
                    Nothing     -> imageLayer
                    Just offset ->
                        let pos = _pos (imageLayer :: RoomImageLayer)
                        in imageLayer {_pos = pos `vecAdd` offset} :: RoomImageLayer
                | (i, imageLayer) <- zip [0..] imageLayers
                ]

    return $ (room :: Room)
        { _surfaces    = nonPlatforms ++ platforms'
        , _items       = nonGoldChunks ++ goldChunks'
        , _imageLayers = imageLayers'
        }
