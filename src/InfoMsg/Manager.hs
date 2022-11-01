module InfoMsg.Manager
    ( thinkInfoMsgManager
    ) where

import Data.Maybe (isJust, fromMaybe)
import qualified Data.List as L

import Collision
import Configs
import Configs.All.Enemy
import Enemy as E
import Enemy.Manager
import InfoMsg.Util
import Level.Room.ArenaWalls
import Level.Room.Bounds
import Level.Room.Types
import Level.Types
import Msg
import Player
import Player.EquipmentInfo
import Player.LockOnAim
import Projectile as P
import Projectile.Manager
import Util
import World
import World.Audio
import World.Audio.LayeredMusic.Manager
import World.Surface

playerProjectedGroundHbxHeight = 2000.0 :: Float

thinkInfoMsgManager :: (ConfigsRead m, MsgsWrite ThinkInfoMsgsPhase m) => World -> m ()
thinkInfoMsgManager world =
    let
        player      = _player (world :: World)
        enemies     = enemyManagerRealEnemies $ _enemyManager world
        projectiles = _projectiles $ _projectileManager world
        surfaces    = worldSurfaces world
        room        = _room $ _level (world :: World)
    in do
        enemyCfg <- _enemy <$> readConfigs

        let
            playerInfoMsgs    = processPlayerInfoMsgs player surfaces enemies enemyCfg
            enemyPosMsgs      = processEnemyPosMsgs enemies
            enemyInStasisMsgs = processEnemyInStasisMsgs enemies
            projPosMsgs       = processProjectilePosMsgs projectiles
            roomInfoMsgs      = processRoomInfoMsgs room
            musicInfoMsgs     = processMusicInfoMsgs $ _audio world

        writeMsgs $ playerInfoMsgs ++ enemyPosMsgs ++ enemyInStasisMsgs ++ projPosMsgs ++ roomInfoMsgs ++ musicInfoMsgs

calculatePlayerInfo :: Player -> [Surface] -> PlayerInfo
calculatePlayerInfo player surfaces = PlayerInfo
    { _msgId            = _msgId (player :: Player)
    , _vel              = _vel (player :: Player)
    , _dir              = _dir (player :: Player)
    , _hitbox           = playerHitbox player
    , _groundBeneathPos = beneathGroundPos
    , _touchingGround   = touchingGround
    , _touchingWall     = _touchingWall (playerFlags :: PlayerFlags)
    , _equipment        = mkPlayerEquipmentInfo player
    , _meter            = _meter (player :: Player)
    , _enemyLockOn      = _enemyLockOn (_lockOnAim player :: PlayerLockOnAim)
    }
    where
        playerPos@(Pos2 playerX playerY) = _pos (player :: Player)

        -- Psuedo ray cast (downwards extended player hitbox) to determine ground pos
        processGroundPos :: Surface -> Maybe Pos2 -> Maybe Pos2
        processGroundPos surface !groundPos
            | intersectsSurface && surfaceTop > playerY =
                let groundPos' = Just $ Pos2 playerX surfaceTop
                in case groundPos of
                    Nothing                    -> groundPos'
                    Just (Pos2 _ groundY)
                        | surfaceTop < groundY -> groundPos'
                        | otherwise            -> groundPos
            | otherwise                                 = groundPos
            where
                playerHbx        = playerHitbox player
                playerHbxTopLeft = hitboxTopLeft playerHbx
                playerHbxWidth   = hitboxWidth playerHbx
                playerProjHbx    = rectHitbox playerHbxTopLeft playerHbxWidth playerProjectedGroundHbxHeight

                surfaceHbx        = _hitbox (surface :: Surface)
                intersectsSurface = playerProjHbx `intersectsHitbox` surfaceHbx
                surfaceTop        = hitboxTop surfaceHbx

        playerFlags          = _flags (player :: Player)
        touchingGround       = _touchingGround (playerFlags :: PlayerFlags)
        beneathGroundPos
            | touchingGround = playerPos
            | otherwise      = fromMaybe playerPos (foldr processGroundPos Nothing surfaces)

processPlayerInfoMsgs :: Player -> [Surface] -> [Some Enemy] -> EnemyConfig -> [Msg ThinkInfoMsgsPhase]
processPlayerInfoMsgs player surfaces enemies enemyCfg = playerInfoMsg:(L.foldl' processSight [] enemies)
    where
        playerInfo    = calculatePlayerInfo player surfaces
        playerInfoMsg = mkMsg $ InfoMsgPlayer playerInfo

        processSight :: [Msg ThinkInfoMsgsPhase] -> Some Enemy -> [Msg ThinkInfoMsgsPhase]
        processSight !ms (Some e) = (mkMsgTo (InfoMsgSeenPlayer seenlayerInfo) enId):ms
            where
                plSightPos      = playerSightPos player
                enId            = E._msgId e
                enSightPos      = enemySightPos e
                eSightRangeSq   = _sightRange enemyCfg ** 2
                outOfSightRange = vecDistSq enSightPos plSightPos > eSightRangeSq

                sightBlocked = any isJust $
                    [ intersectsLineHitbox enSightPos plSightPos hitbox
                    | surface <- surfaces
                    , let hitbox = _hitbox (surface :: Surface)
                    ]

                seenlayerInfo
                    | outOfSightRange || sightBlocked = Nothing
                    | otherwise                       = Just playerInfo

processEnemyPosMsgs :: [Some Enemy] -> [Msg ThinkInfoMsgsPhase]
processEnemyPosMsgs enemies =
    [ mkMsg (InfoMsgEnemyPos hbx enemyId)
    | Some enemy <- enemies
    , let hbx     = enemyHitbox enemy
    , let enemyId = E._msgId enemy
    ]

processEnemyInStasisMsgs :: [Some Enemy] -> [Msg ThinkInfoMsgsPhase]
processEnemyInStasisMsgs enemies =
    [ mkMsg $ InfoMsgEnemyInStasis (E._msgId enemy)
    | Some enemy <- enemies
    , isEnemyInStasis enemy
    ]

processProjectilePosMsgs :: [Some Projectile] -> [Msg ThinkInfoMsgsPhase]
processProjectilePosMsgs projectiles =
    [ mkMsg (InfoMsgProjectilePos projPos projOwnerId projId)
    | Some proj <- projectiles
    , let projPos     = hitboxTopLeft $ projectileHitbox proj
    , let projId      = P._msgId proj
    , let projOwnerId = _ownerId proj
    ]

processRoomInfoMsgs :: Room -> [Msg ThinkInfoMsgsPhase]
processRoomInfoMsgs room = boundsMsg:arenaWallsMsgs
    where
        topBounds = _topBounds (_bounds room :: RoomBounds)
        boundsMsg = mkMsg $ InfoMsgRoomTopBounds topBounds

        arenaWallsMsgs = case _arenaWalls room of
            Just arenaWalls
                | isRoomArenaWallsActive arenaWalls ->
                    let
                        arenaWallsInfo = RoomArenaWallsInfo
                            { _leftWallPos  = roomArenaWallsLeftWallPos arenaWalls
                            , _rightWallPos = roomArenaWallsRightWallPos arenaWalls
                            }
                    in [mkMsg $ InfoMsgRoomArenaWalls arenaWallsInfo]

            _ -> []

processMusicInfoMsgs :: WorldAudio -> [Msg ThinkInfoMsgsPhase]
processMusicInfoMsgs worldAudio =
    [ mkMsg $ InfoMsgBattleMusic battleMusicType
    , mkMsg $ InfoMsgExplorationMusic explorationMusicType
    ]
    where
        layeredMusicManager  = _layeredMusicManager worldAudio
        battleMusicType      = _type (_battleMusic layeredMusicManager :: LayeredMusic)
        explorationMusicType = _type (_explorationMusic layeredMusicManager :: LayeredMusic)
