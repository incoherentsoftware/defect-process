module Level.Room.ArenaWalls.Util
    ( wallDisappearSoundPath
    , wallDisappearSprPath
    , roomArenaWallsMarkerPos
    , calculateRoomArenaWallsGoldValue
    , roomArenaWallsBoundingHitbox
    , roomArenaWallsLeftWallPos
    , roomArenaWallsRightWallPos
    , roomArenaWallsDisappearMsgs
    ) where

import qualified Data.List.NonEmpty as NE

import Collision.Hitbox
import Configs.All.Level
import FileCache
import Level.DangerValue
import Level.Room.ArenaWalls.JSON
import Level.Room.ArenaWalls.Types
import Msg
import Particle.All.Simple
import Util
import World.GoldDrop
import World.Surface
import World.Util
import World.ZIndex

wallSprHitboxOffset = Pos2 55.0 0.0 :: Pos2

wallDisappearSprPath   =
    PackResourceFilePath "data/levels/level-arena.pack" "arena-wall-disappear.spr" :: PackResourceFilePath
wallDisappearSoundPath = "event:/SFX Events/Level/arena-wall-disappear"            :: FilePath

roomArenaWallsMarkerPos :: RoomArenaWalls -> Pos2
roomArenaWallsMarkerPos arenaWalls = Pos2 (hitboxLeft triggerHbx) (hitboxBot triggerHbx)
    where triggerHbx = _triggerHitbox (arenaWalls :: RoomArenaWalls)

calculateRoomArenaWallsGoldValue :: DangerValue -> LevelConfig -> GoldValue
calculateRoomArenaWallsGoldValue dangerVal cfg = case goldValues of
    (goldValue:_) -> goldValue
    []            -> _goldValue (NE.last arenaWallsGoldDrops :: RoomArenaWallsGoldDropJSON)
    where
        arenaWallsGoldDrops = _arenaWallsGoldDrops cfg
        goldValues          =
            [ _goldValue (json :: RoomArenaWallsGoldDropJSON)
            | json <- NE.toList arenaWallsGoldDrops
            , dangerVal <= _dangerValue (json :: RoomArenaWallsGoldDropJSON)
            ]

roomArenaWallsBoundingHitbox :: RoomArenaWalls -> Hitbox
roomArenaWallsBoundingHitbox arenaWalls = rectHitbox (Pos2 x y) width height
    where
        leftWallHbx  = _hitbox $ _leftWall (arenaWalls :: RoomArenaWalls)
        rightWallHbx = _hitbox $ _rightWall (arenaWalls :: RoomArenaWalls)
        x            = hitboxLeft leftWallHbx
        y            = min (hitboxTop leftWallHbx) (hitboxTop rightWallHbx)
        width        = hitboxRight rightWallHbx - hitboxLeft leftWallHbx
        height       = max (hitboxHeight leftWallHbx) (hitboxHeight rightWallHbx)

roomArenaWallsLeftWallPos :: RoomArenaWalls -> Pos2
roomArenaWallsLeftWallPos arenaWalls = hitboxBotRight leftWallHbx `vecSub` wallSprHitboxOffset
    where leftWallHbx = _hitbox $ _leftWall (arenaWalls :: RoomArenaWalls)

roomArenaWallsRightWallPos :: RoomArenaWalls -> Pos2
roomArenaWallsRightWallPos arenaWalls = hitboxBotLeft rightWallHbx `vecAdd` wallSprHitboxOffset
    where rightWallHbx = _hitbox $ _rightWall (arenaWalls :: RoomArenaWalls)

roomArenaWallsDisappearMsgs :: RoomArenaWalls -> [Msg ThinkLevelMsgsPhase]
roomArenaWallsDisappearMsgs arenaWalls = goldMsg:(audioMsgs ++ particleMsgs)
    where
        leftWallPos@(Pos2 leftWallX leftWallY) = roomArenaWallsLeftWallPos arenaWalls
        rightWallPos@(Pos2 rightWallX _)       = roomArenaWallsRightWallPos arenaWalls
        wallPositions                          = [leftWallPos, rightWallPos]

        audioMsgs    = map (mkMsg . AudioMsgPlaySound wallDisappearSoundPath) wallPositions
        loadParticle = \pos -> loadSimpleParticle pos RightDir levelArenaWallsZIndex wallDisappearSprPath
        particleMsgs = map (mkMsg . ParticleMsgAddM . loadParticle) wallPositions

        centerPos = Pos2 (leftWallX + (rightWallX - leftWallX) / 2.0) leftWallY
        goldValue = _goldValue (arenaWalls :: RoomArenaWalls)
        goldMsg   = mkMsg $ NewThinkProjectileMsgAddsM (mkArenaGoldDrops centerPos goldValue)
