module Level.Room.ArenaWalls
    ( module Level.Room.ArenaWalls.Types
    , roomArenaWallsLeftWallPos
    , roomArenaWallsRightWallPos
    , wallAppearSoundPath
    , mkRoomArenaWallsLeftRightWalls
    , mkRoomArenaWalls
    , isRoomArenaWallsReady
    , isRoomArenaWallsActive
    , isRoomArenaWallsDone
    , thinkRoomArenaWalls
    , updateRoomArenaWalls
    , drawRoomArenaWalls
    , preloadRoomArenaWallsPackResources
    , roomArenaWallsSurfaces
    , roomArenaWallsTriggerPlayerCollision
    , roomArenaWallsTriggerPlayerAttackCollision
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (traverse_)
import Data.Functor           ((<&>))
import Data.Maybe             (fromMaybe, listToMaybe)
import qualified Data.List as L
import qualified Data.Set as S

import AppEnv
import Async.Request
import Collision.Hitbox
import Configs
import Configs.All.Level
import Configs.All.Settings
import Configs.All.Settings.Debug
import Enemy.All
import FileCache
import Id
import Level.DangerValue
import Level.Room.ArenaWalls.EnemySpawn
import Level.Room.ArenaWalls.JSON
import Level.Room.ArenaWalls.Ripple
import Level.Room.ArenaWalls.Types
import Level.Room.ArenaWalls.Util
import Msg
import Particle.All.Simple
import Util
import Window.Graphics
import World.Surface
import World.ZIndex

debugWallHitboxColor         = Color 255 0 220 150   :: Color
debugInactiveWallHitboxColor = Color 255 0 220 50    :: Color
debugTriggerHitboxColor      = Color 200 200 200 150 :: Color

wallsDrawScale           = Scaled 1.2 :: DrawScale
markerDrawScale          = Scaled 1.3 :: DrawScale
minEnemySpawnPosWallDist = 131        :: Distance

packPath               = \p -> PackResourceFilePath "data/levels/level-arena.pack" p
markerIdleSprPath      = packPath "arena-marker-idle.spr"      :: PackResourceFilePath
markerDisappearSprPath = packPath "arena-marker-disappear.spr" :: PackResourceFilePath

markerIdleSoundPath      = "event:/SFX Events/Level/arena-marker-idle-c"    :: FilePath
markerRippleSoundPath    = "event:/SFX Events/Level/arena-marker-ripple"    :: FilePath
markerDisappearSoundPath = "event:/SFX Events/Level/arena-marker-disappear" :: FilePath
wallAppearSoundPath      = "event:/SFX Events/Level/arena-wall-appear"      :: FilePath

enemyParticlesPackPath = "data/particles/particles-enemy.pack" :: FilePath

mkRoomArenaWallsSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m RoomArenaWallsSprites
mkRoomArenaWallsSprites =
    RoomArenaWallsSprites <$>
    loadPackSpr "arena-wall-appear.spr" <*>
    loadPackSpr "arena-wall-idle.spr" <*>
    loadPackSpr "arena-wall-hit.spr" <*>
    loadPackSpr "arena-marker-ripple.spr"
    where loadPackSpr = \f -> loadPackSprite $ packPath f

mkRoomArenaWallsLeftRightWalls :: DangerValue -> LevelConfig -> RoomArenaWallsJSON -> (Surface, Surface)
mkRoomArenaWallsLeftRightWalls currentDangerValue cfg wallsJSON =
    ( moveSurface (Pos2 offsetX 0.0) leftWall
    , moveSurface (Pos2 (-offsetX) 0.0) rightWall
    )
    where
        leftWall  = mkGeneralSurface . _fromJSON $ _leftWall (wallsJSON :: RoomArenaWallsJSON)
        rightWall = mkGeneralSurface . _fromJSON $ _rightWall (wallsJSON :: RoomArenaWallsJSON)

        wallWidth    = hitboxLeft (_hitbox rightWall) - hitboxRight (_hitbox leftWall)
        maxWallWidth = fromMaybe wallWidth $ listToMaybe
            [ _maxWidth maxWidthJSON
            | maxWidthJSON <- _arenaWallsMaxWidths cfg
            , currentDangerValue <= _maxDangerValue maxWidthJSON
            ]

        offsetX
            | wallWidth <= maxWallWidth = 0.0  -- RoomArenaWallsJSON defines max bounds for the level, don't exceed
            | otherwise                 = (wallWidth - maxWallWidth) / 2.0

limitEnemySpawnPositions :: Surface -> Surface -> [Pos2] -> [Pos2]
limitEnemySpawnPositions leftWall rightWall spawnPositions = filter withinWalls spawnPositions
    where
        withinWalls = \(Pos2 spawnX _) ->
            let
                innerLeftX  = hitboxRight $ _hitbox leftWall
                innerRightX = hitboxLeft $ _hitbox rightWall
            in spawnX - innerLeftX >= minEnemySpawnPosWallDist && innerRightX - spawnX >= minEnemySpawnPosWallDist

mkRoomArenaWalls
    :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m)
    => DangerValue
    -> RoomArenaWallsJSON
    -> m RoomArenaWalls
mkRoomArenaWalls currentDangerValue wallsJSON = do
    cfg <- _level <$> readConfigs
    let
        (leftWall', rightWall') = mkRoomArenaWallsLeftRightWalls currentDangerValue cfg wallsJSON
        enemySpawnPositions     = _enemySpawnPositions (wallsJSON :: RoomArenaWallsJSON)
        enemySpawnPositions'    = limitEnemySpawnPositions leftWall' rightWall' enemySpawnPositions
        triggerHbx              = _fromJSON $ _triggerHitbox (wallsJSON :: RoomArenaWallsJSON)
        goldValue               = calculateRoomArenaWallsGoldValue currentDangerValue cfg

    hashedId <- hashId <$> newId
    status   <- readSettingsConfig _debug _disableRoomArenaWallsTrigger <&> \case
        False -> WallsReadyStatus
        True  -> WallsDoneStatus

    enemySpawnWaves <- chooseRoomArenaWallsEnemySpawnWaves currentDangerValue
    markerSpr       <- loadPackSprite markerIdleSprPath
    sprs            <- mkRoomArenaWallsSprites

    return $ RoomArenaWalls
        { _hashedId              = hashedId
        , _status                = status
        , _leftWall              = leftWall'
        , _rightWall             = rightWall'
        , _triggerHitbox         = triggerHbx
        , _triggerHitByHashedIds = S.empty
        , _enemySpawnPositions   = enemySpawnPositions'
        , _enemySpawnWaves       = enemySpawnWaves
        , _goldValue             = goldValue
        , _markerRipples         = []
        , _markerSprite          = markerSpr
        , _wallSprite            = _wallAppear sprs
        , _sprites               = sprs
        }

isRoomArenaWallsReady :: RoomArenaWalls -> Bool
isRoomArenaWallsReady = (== WallsReadyStatus) . _status

isRoomArenaWallsActive :: RoomArenaWalls -> Bool
isRoomArenaWallsActive arenaWalls = case _status arenaWalls of
    WallsActiveStatus _ -> True
    _                   -> False

isRoomArenaWallsDone :: RoomArenaWalls -> Bool
isRoomArenaWallsDone = (== WallsDoneStatus) . _status

thinkRoomArenaWalls :: MsgsRead ThinkLevelMsgsPhase m => RoomArenaWalls -> m [Msg ThinkLevelMsgsPhase]
thinkRoomArenaWalls arenaWalls = thinkRoomArenaWallsEnemySpawn arenaWalls

updateRoomArenaWalls :: MsgsReadWrite UpdateLevelMsgsPhase m => RoomArenaWalls -> m RoomArenaWalls
updateRoomArenaWalls arenaWalls = case status of
    WallsReadyStatus -> do
        let
            markerPos = roomArenaWallsMarkerPos arenaWalls
            hashedId  = _hashedId arenaWalls
        writeMsgs [mkMsg $ AudioMsgPlaySoundContinuous markerIdleSoundPath hashedId markerPos]

        let
            arenaWalls' = arenaWalls
                { _markerRipples = markerRipples
                , _markerSprite  = updateSprite $ _markerSprite arenaWalls
                }
        L.foldl' processUpdateMsg arenaWalls' <$> readMsgs

    WallsActiveStatus _ ->
        let
            processWallsSplatMsgs :: [RoomMsgPayload] -> Maybe Pos2
            processWallsSplatMsgs []     = Nothing
            processWallsSplatMsgs (p:ps) = case p of
                RoomMsgArenaWallsSplat pos -> Just pos
                _                          -> processWallsSplatMsgs ps

            wallSpr    = _wallSprite arenaWalls
            sprs       = _sprites arenaWalls
            wallHitSpr = _wallHit sprs
        in do
            msgs <- readMsgs

            arenaWalls' <- case processWallsSplatMsgs msgs of
                Nothing ->
                    let
                        wallIdleSpr     = _wallIdle sprs
                        wallSprFinished = spriteFinished wallSpr

                        wallSpr'
                            | wallSpr == _wallAppear sprs && wallSprFinished = wallIdleSpr
                            | wallSpr == wallHitSpr && wallSprFinished       = wallIdleSpr
                            | otherwise                                      = updateSprite wallSpr
                    in return $ arenaWalls
                        { _markerRipples = markerRipples
                        , _wallSprite    = updateSprite wallSpr'
                        }

                Just wallsSplatPos -> do
                    writeMsgs $ roomArenaWallsWallSplatMsgs wallsSplatPos
                    return $ arenaWalls
                        { _markerRipples = markerRipples
                        , _wallSprite    = wallHitSpr
                        }

            return $ L.foldl' processUpdateMsg arenaWalls' msgs

    WallsDoneStatus -> return arenaWalls

    where
        processUpdateMsg :: RoomArenaWalls -> RoomMsgPayload -> RoomArenaWalls
        processUpdateMsg aw p = case p of
            RoomMsgUpdateArenaWalls update -> update aw
            _                              -> aw

        status        = _status arenaWalls
        markerRipples = updateRoomArenaMarkerRipples status (_markerRipples arenaWalls)

drawRoomArenaWalls :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => RoomArenaWalls -> m ()
drawRoomArenaWalls arenaWalls =
    let
        active          = isRoomArenaWallsActive arenaWalls
        debugWallColor
            | active    = debugWallHitboxColor
            | otherwise = debugInactiveWallHitboxColor

        leftWallHbx  = _hitbox $ _leftWall (arenaWalls :: RoomArenaWalls)
        rightWallHbx = _hitbox $ _rightWall (arenaWalls :: RoomArenaWalls)
        triggerHbx   = _triggerHitbox (arenaWalls :: RoomArenaWalls)
    in do
        whenM (readSettingsConfig _debug _drawArenaHitboxes) $ if
            | active    -> do
                drawHitbox debugWallColor debugHitboxZIndex leftWallHbx
                drawHitbox debugWallColor debugHitboxZIndex rightWallHbx
            | otherwise -> drawHitbox debugTriggerHitboxColor debugHitboxZIndex triggerHbx

        case _status arenaWalls of
            WallsReadyStatus ->
                let
                    markerPos = roomArenaWallsMarkerPos arenaWalls
                    markerSpr = _markerSprite arenaWalls
                in do
                    drawSpriteEx markerPos RightDir levelArenaWallsZIndex 0.0 FullOpacity markerDrawScale markerSpr
                    drawRoomArenaMarkerRipples $ _markerRipples arenaWalls

            WallsActiveStatus _ ->
                let
                    leftWallPos  = roomArenaWallsLeftWallPos arenaWalls
                    rightWallPos = roomArenaWallsRightWallPos arenaWalls
                    wallSpr      = _wallSprite arenaWalls
                in do
                    drawSpriteEx leftWallPos LeftDir levelArenaWallsZIndex 0.0 FullOpacity wallsDrawScale wallSpr
                    drawSpriteEx rightWallPos RightDir levelArenaWallsZIndex 0.0 FullOpacity wallsDrawScale wallSpr
                    drawRoomArenaMarkerRipples $ _markerRipples arenaWalls

            WallsDoneStatus -> return ()

preloadRoomArenaWallsPackResources :: RoomArenaWalls -> AppEnv p ()
preloadRoomArenaWallsPackResources arenaWalls =
    let
        enemySpawnWaves           = _enemySpawnWaves (arenaWalls :: RoomArenaWalls)
        enemyTypes                = S.fromList $ concatMap _enemyTypes enemySpawnWaves
        enemyPreloadPackFilePaths = concat $ S.toList (S.map enemyPreloadPackFilePathsFromType enemyTypes)
        writeAsyncRequest'        = writeAsyncRequest . PreloadPackFileRequest
    in do
        traverse_ writeAsyncRequest' enemyPreloadPackFilePaths
        writeAsyncRequest' enemyParticlesPackPath

roomArenaWallsSurfaces :: RoomArenaWalls -> [Surface]
roomArenaWallsSurfaces arenaWalls
    | isRoomArenaWallsActive arenaWalls =
        [ _leftWall (arenaWalls :: RoomArenaWalls)
        , _rightWall (arenaWalls :: RoomArenaWalls)
        ]
    | otherwise                         = []

roomArenaWallsTriggerPlayerCollision :: RoomArenaWalls -> [Msg ThinkCollisionMsgsPhase]
roomArenaWallsTriggerPlayerCollision arenaWalls = case _status arenaWalls of
    WallsReadyStatus ->
        let
            markerPos                  = roomArenaWallsMarkerPos arenaWalls
            markerDisappearParticleMsg =
                let mkParticle = loadSimpleParticle markerPos RightDir levelArenaWallsZIndex markerDisappearSprPath
                in mkMsg $ ParticleMsgAddM mkParticle
            updateStatus               = \aw -> aw {_status = WallsActiveStatus 0.0}
        in
            [ mkMsg $ RoomMsgUpdateArenaWalls updateStatus
            , markerDisappearParticleMsg
            , mkMsg $ AudioMsgPlaySound markerDisappearSoundPath markerPos
            , mkMsg $ AudioMsgPlaySound wallAppearSoundPath (roomArenaWallsLeftWallPos arenaWalls)
            , mkMsg $ AudioMsgPlaySound wallAppearSoundPath (roomArenaWallsRightWallPos arenaWalls)
            , mkMsg AudioMsgRampMusicToNormalVolume
            ]

    _ -> []

roomArenaWallsTriggerPlayerAttackCollision :: S.Set HashedId -> RoomArenaWalls -> [Msg ThinkCollisionMsgsPhase]
roomArenaWallsTriggerPlayerAttackCollision hashedIds arenaWalls = case _status arenaWalls of
    WallsReadyStatus
        | not (S.null hashedIds) && not (hashedIds `S.isSubsetOf` triggerHitByHashedIds) ->
            let
                updateArenaWallsMsg = mkMsg . RoomMsgUpdateArenaWalls $ \aw -> aw
                    { _triggerHitByHashedIds = hashedIds `S.union` triggerHitByHashedIds
                    , _markerRipples         = mkRoomArenaMarkerRipple aw:_markerRipples aw
                    }
            in
                [ updateArenaWallsMsg
                , mkMsg $ AudioMsgPlaySound markerRippleSoundPath (roomArenaWallsMarkerPos arenaWalls)
                ]

    _ -> []

    where triggerHitByHashedIds = _triggerHitByHashedIds arenaWalls
