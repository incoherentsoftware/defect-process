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
import Level.Room
import Level.Room.ArenaWalls
import Level.Room.Item
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
        room        = _room $ _level (world :: World)
    in do
        enemyCfg <- _enemy <$> readConfigs

        writeMsgs $ concat
            [ processPlayerInfoMsgs player enemies room enemyCfg
            , processEnemyPosMsgs enemies
            , processEnemyInStasisMsgs enemies
            , processEnemyInHitstunMsgs enemies
            , processProjectilePosMsgs projectiles
            , processRoomInfoMsgs room
            , processMusicInfoMsgs $ _audio world
            ]

processPlayerInItemInteractRange :: Room -> Bool
processPlayerInItemInteractRange room = or
    [ (_inInteractRange item) item
    | Some item <- _items room
    ]

processPlayerGroundPos :: Player -> Room -> Pos2
processPlayerGroundPos player room = fromMaybe playerPos (foldr processGroundPos Nothing surfaces)
    where
        playerPos@(Pos2 playerX playerY) = _pos (player :: Player)
        surfaces                         = roomSurfaces room
        playerHbx                        = playerHitbox player
        playerHbxTopLeft                 = hitboxTopLeft playerHbx
        playerHbxWidth                   = hitboxWidth playerHbx
        playerProjHbx                    = rectHitbox playerHbxTopLeft playerHbxWidth playerProjectedGroundHbxHeight

        -- psuedo ray cast (downwards extended player hitbox) to determine ground pos
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
                surfaceHbx        = _hitbox (surface :: Surface)
                intersectsSurface = playerProjHbx `intersectsHitbox` surfaceHbx
                surfaceTop        = hitboxTop surfaceHbx

calculatePlayerInfo :: Player -> Room -> PlayerInfo
calculatePlayerInfo player room = PlayerInfo
    { _msgId               = _msgId (player :: Player)
    , _vel                 = _vel (player :: Player)
    , _dir                 = _dir (player :: Player)
    , _hitbox              = playerHitbox player
    , _groundBeneathPos    = beneathGroundPos
    , _touchingGround      = touchingGround
    , _touchingWall        = _touchingWall (playerFlags :: PlayerFlags)
    , _onPlatform          = _onPlatform (playerFlags :: PlayerFlags)
    , _inItemInteractRange = processPlayerInItemInteractRange room
    , _equipment           = mkPlayerEquipmentInfo player
    , _meter               = _meter (player :: Player)
    , _enemyLockOn         = _enemyLockOn (_lockOnAim player :: PlayerLockOnAim)
    }
    where
        playerFlags          = _flags (player :: Player)
        touchingGround       = _touchingGround (playerFlags :: PlayerFlags)
        beneathGroundPos
            | touchingGround = _pos (player :: Player)
            | otherwise      = processPlayerGroundPos player room

processPlayerInfoMsgs :: Player -> [Some Enemy] -> Room -> EnemyConfig -> [Msg ThinkInfoMsgsPhase]
processPlayerInfoMsgs player enemies room enemyCfg = playerInfoMsg:(L.foldl' processSight [] enemies)
    where
        playerInfo    = calculatePlayerInfo player room
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
                    | surface <- roomSurfaces room
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

processEnemyInHitstunMsgs :: [Some Enemy] -> [Msg ThinkInfoMsgsPhase]
processEnemyInHitstunMsgs enemies =
    [ mkMsg $ InfoMsgEnemyInHitstun (E._msgId enemy)
    | Some enemy <- enemies
    , isEnemyInHitstun enemy
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
