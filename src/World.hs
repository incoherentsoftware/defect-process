module World
    ( module World.Types
    , mkWorld
    , resetWorld
    , worldSurfaces
    , worldIsHitlag
    , changeWorldRoom
    , changeWorldRoomNoSkip
    , worldPlayerEquipmentInfo
    , isWorldPlayerTouchingInfoSign
    , setWorldPlayerSecondarySkillManagerOrder
    ) where

import Control.Concurrent     (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as M
import qualified Data.Set as S

import AppEnv
import Async.Request
import Async.Signal
import Configs
import Configs.All.Level
import Enemy.Manager
import Level
import Level.Room
import Level.Room.Chooser
import Level.Room.ImageLayer
import Level.Room.Parse
import Msg
import Particle.Manager
import Player
import Player.EquipmentInfo
import Player.RoomInfo
import Player.SecondarySkill.Types
import Projectile.Manager
import Stats.Manager
import Util
import Util.Time
import Window.Graphics
import World.Audio
import World.Camera
import World.RunProgressScreen
import World.Screenshake
import World.Surface
import World.Types
import World.UI

data ChangeRoomBehavior
    = OnMissingRoomSkip
    | OnMissingRoomError

mkWorldInternal :: WorldAudio -> AppEnv SetupMsgsPhase World
mkWorldInternal audio = do
    player            <- mkPlayer
    ui                <- mkWorldUI
    level             <- mkLevel
    runProgressScreen <- mkRunProgressScreen

    return $ World
        { _player            = player
        , _level             = level
        , _enemyManager      = mkEnemyManager
        , _projectileManager = mkProjectileManager
        , _particleManager   = mkParticleManager
        , _statsManager      = mkStatsManager
        , _audio             = audio
        , _ui                = ui
        , _runProgressScreen = runProgressScreen
        , _hitlagTtl         = 0.0
        , _screenshake       = mkScreenshake 0.0
        , _camera            = mkWorldCamera
        , _status            = WorldInitialStatus
        , _levelLoadSecs     = 0.0
        , _screenWipe        = Nothing
        , _pendingChange     = Nothing
        }

mkWorld :: AppEnv SetupMsgsPhase World
mkWorld = do
    world <- mkWorldInternal =<< mkWorldAudio
    changeWorldRoom startingShopRoomType 0.0 world

resetWorld :: World -> AppEnv SetupMsgsPhase World
resetWorld world = do
    world' <- case _status world of
        WorldInitialStatus -> return world
        _                  -> do
            w <- mkWorldInternal =<< resetWorldAudio (_audio (world :: World))
            changeWorldRoom startingShopRoomType 0.0 w
    setGraphicsCursor _crosshair
    return world'

worldSurfaces :: World -> [Surface]
worldSurfaces = roomSurfaces . _room . (_level :: World -> Level)

worldIsHitlag :: World -> Bool
worldIsHitlag = (> 0.0) . _hitlagTtl

sleepForRunProgressScreen :: (ConfigsRead m, MonadIO m) => RoomType -> Secs -> m Secs
sleepForRunProgressScreen roomType levelLoadSecs = do
    let isRunProgressScreenRoomType = isArenaRoomType roomType || roomType == endRoomType
    runProgressScreenSecs          <- readConfig _level _runProgressScreenSecs
    if
        | not isRunProgressScreenRoomType || levelLoadSecs >= runProgressScreenSecs -> return levelLoadSecs
        | otherwise                                                                 -> do
            sleepTime <- mkTime
            liftIO . threadDelay . round $ (runProgressScreenSecs - levelLoadSecs) * 1000000
            (levelLoadSecs +) . _diffSecs <$> updateTime sleepTime

changeWorldRoomInternal :: RoomType -> PosY -> ChangeRoomBehavior -> World -> AppEnv p World
changeWorldRoomInternal roomType playerOffsetY changeRoomBehavior world = do
    let
        level              = _level (world :: World)
        playerRoomInfo     = mkPlayerRoomInfo $ _player (world :: World)
        roomChooser        = _roomChooser level
        currentDangerValue = _currentDangerValue level
        statsManager       = _statsManager world

        loadRoom' = \rId -> loadRoom rId playerRoomInfo roomChooser currentDangerValue statsManager

    loadTime <- mkTime

    room <- catchAppEnv (Right <$> loadRoom' roomType) (return . Left) >>= \case
        Right r -> return r
        Left e  -> case changeRoomBehavior of
            OnMissingRoomError -> error $ show e
            OnMissingRoomSkip  -> loadRoom' $ nextRoomChooserRoomTypeSkipTransition roomChooser

    let
        room' = case _type (room :: Room) `M.lookup` (_roomItemsOverride level) of
            Nothing -> room
            Just ri -> room {_items = ri}
    level' <- changeLevelRoom room' level

    let
        roomChooser' = _roomChooser level'
        nextRoomType = nextRoomChooserRoomType roomChooser'
    writeAsyncRequest $ PreloadRoomFgBgPackFilesRequest nextRoomType
    writeAsyncSignal DoMainThreadLoadsSignal

    let
        roomTextures        = \r ->
            map ((_texture :: Image -> Texture) . (_image :: RoomImageLayer -> Image)) (_imageLayers r)
        prevRoomTextures    = S.fromList . roomTextures . _room $ _level (world :: World)
        currentRoomTextures = S.fromList $ roomTextures room'
        oldTextures         = S.filter (`S.notMember` currentRoomTextures) prevRoomTextures
    freeTextures oldTextures

    let
        player           = resetPlayerOnChangeWorldRoom room' playerOffsetY (_player (world :: World))
        numVisitedArenas = S.size $ _visitedArenaRoomNames roomChooser'
        numArenas        = _numArenas roomChooser'
        currentRoomType  = _currentRoomType roomChooser'
        audio            = setWorldAudioRoomInfo currentRoomType numVisitedArenas numArenas (_audio (world :: World))
        worldUI          = resetWorldUIOnChangeWorldRoom $ _ui world

    levelLoadSecs  <- _diffSecs <$> updateTime loadTime
    levelLoadSecs' <- sleepForRunProgressScreen currentRoomType levelLoadSecs

    return $ world
        { _level             = level'
        , _enemyManager      = mkEnemyManager
        , _player            = player
        , _projectileManager = mkProjectileManager
        , _particleManager   = mkParticleManager
        , _audio             = audio
        , _ui                = worldUI
        , _camera            = mkWorldCamera
        , _levelLoadSecs     = levelLoadSecs'
        }

changeWorldRoom :: RoomType -> PosY -> World -> AppEnv p World
changeWorldRoom roomType playerOffsetY world = changeWorldRoomInternal roomType playerOffsetY OnMissingRoomSkip world

changeWorldRoomNoSkip :: RoomType -> PosY -> World -> AppEnv p World
changeWorldRoomNoSkip roomType playerOffsetY world =
    changeWorldRoomInternal roomType playerOffsetY OnMissingRoomError world

setWorldPlayerSecondarySkillManagerOrder
    :: Maybe SecondarySkillType
    -> Maybe SecondarySkillType
    -> Maybe SecondarySkillType
    -> World
    -> World
setWorldPlayerSecondarySkillManagerOrder neutralSlotType upSlotType downSlotType world = world {_player = player'}
    where
        player  = _player (world :: World)
        player' = setPlayerSecondarySkillManagerOrder neutralSlotType upSlotType downSlotType player

worldPlayerEquipmentInfo :: World -> PlayerEquipmentInfo
worldPlayerEquipmentInfo world = mkPlayerEquipmentInfo $ _player (world :: World)

isWorldPlayerTouchingInfoSign :: World -> Bool
isWorldPlayerTouchingInfoSign = _touchingInfoSign . _flags . (_player :: World -> Player)
