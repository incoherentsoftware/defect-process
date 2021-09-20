module Collision.Level
    ( checkRoomPortalPlayerCollisions
    , checkRoomItemCollisions
    , checkRoomSpringLauncherCollisions
    , checkRoomArenaWallsCollisions
    ) where

import Control.Monad       (when)
import Control.Monad.State (execState, modify)
import Data.Maybe          (catMaybes, fromMaybe)
import qualified Data.Set as S

import Attack
import Collision
import Id
import Level.Room
import Level.Room.ArenaWalls
import Level.Room.Item as RI
import Level.Room.SpringLauncher
import Msg
import Player
import Projectile as P
import Util

arenaWallsTriggerProjRegCollisions = S.fromList
    [ ProjRegisteredSurfaceCollision
    , ProjRegisteredEnemyCollision
    , ProjRegisteredEnemyRealCollision
    , ProjRegisteredRoomItemCollision
    ] :: S.Set ProjectileRegisteredCollision

checkRoomPortalPlayerCollisions :: Room -> Player -> [Msg ThinkCollisionMsgsPhase]
checkRoomPortalPlayerCollisions room player = foldr checkCollision [] (roomPortalHitboxes room)
    where
        checkCollision :: Hitbox -> [Msg ThinkCollisionMsgsPhase] -> [Msg ThinkCollisionMsgsPhase]
        checkCollision portalHbx !ms
            | intersectsHitbox playerHbx portalHbx = switchRoomMsg:ms
            | otherwise                            = ms
            where
                playerHbx     = playerHitbox player
                playerOffsetY = min 0.0 (hitboxBot playerHbx - hitboxBot portalHbx)
                switchRoomMsg = mkMsg $ WorldMsgSwitchRoom NextRoomType playerOffsetY

checkRoomItemCollisions :: [Some RoomItem] -> Player -> [Msg ThinkCollisionMsgsPhase]
checkRoomItemCollisions items player = withPlayerMsgs ++ withPlayerAtkMsgs
    where
        withPlayerMsgs    = checkRoomItemPlayerCollisions items player
        withPlayerAtkMsgs = checkRoomItemPlayerAttackCollisions items player

checkRoomItemPlayerCollisions :: [Some RoomItem] -> Player -> [Msg ThinkCollisionMsgsPhase]
checkRoomItemPlayerCollisions [] _ = []
checkRoomItemPlayerCollisions (Some item:items) player
    | playerHbx `intersectsHitbox` itemHbx = playerCollision player item ++ checkRoomItemPlayerCollisions'
    | otherwise                            = checkRoomItemPlayerCollisions'
    where
        playerHbx                      = playerHitbox player
        itemHbx                        = RI._hitbox item
        playerCollision                = _playerCollision item
        checkRoomItemPlayerCollisions' = checkRoomItemPlayerCollisions items player

checkRoomItemPlayerAttackCollisions :: [Some RoomItem] -> Player -> [Msg ThinkCollisionMsgsPhase]
checkRoomItemPlayerAttackCollisions items player = fromMaybe [] $ do
    atk    <- _attack (player :: Player)
    atkHbx <- attackHitbox atk

    let
        checkCollision :: Some RoomItem -> [Msg ThinkCollisionMsgsPhase] -> [Msg ThinkCollisionMsgsPhase]
        checkCollision (Some ri) !ms
            | _isAttackable ri && atkHbx `intersectsHitbox` RI._hitbox ri =
                attackCollisionEntityHitMessages ri atk ++ ms
            | otherwise                                                   = ms

    Just $ foldr checkCollision [] items

checkRoomSpringLauncherCollisions :: Room -> Player -> [Msg ThinkCollisionMsgsPhase]
checkRoomSpringLauncherCollisions room player = foldr checkCollision [] (_springLaunchers room)
    where
        checkCollision :: SpringLauncher -> [Msg ThinkCollisionMsgsPhase] -> [Msg ThinkCollisionMsgsPhase]
        checkCollision springLauncher !ms
            | playerHbx `intersectsHitbox` springLauncherHbx = springLauncherPlayerCollision springLauncher ++ ms
            | otherwise                                      = ms
            where
                playerHbx         = playerHitbox player
                springLauncherHbx = springLauncherHitbox springLauncher

checkRoomArenaWallsCollisions :: Room -> [Some Projectile] -> Player -> [Msg ThinkCollisionMsgsPhase]
checkRoomArenaWallsCollisions room projectiles player = case _arenaWalls room of
    Just arenaWalls
        | isRoomArenaWallsReady arenaWalls -> flip execState [] $ do
            let triggerHbx = _triggerHitbox arenaWalls
            when (hitboxCenter (playerHitbox player) `containsPointHitbox` triggerHbx) $
                modify (roomArenaWallsTriggerPlayerCollision arenaWalls ++)

            case _attack (player :: Player) of
                Just atk
                    | maybe False (`intersectsHitbox` triggerHbx) (attackHitbox atk) ->
                        let atkHashedIds = S.fromList [hashId $ _id atk]
                        in modify (roomArenaWallsTriggerPlayerAttackCollision atkHashedIds arenaWalls ++)
                _                                                                    -> return ()

            -- intentionally don't call _processCollisions on projs, don't want the standard collision response
            let
                checkTriggerProjCollision :: Some Projectile -> Maybe HashedId
                checkTriggerProjCollision (Some proj)
                    | projectileHitbox proj `intersectsHitbox` triggerHbx = Just $ hashId (P._msgId proj)
                    | otherwise                                           = Nothing

                inTriggerProjRegCollisions = \(Some p) -> not . S.null $
                    S.filter (`S.member` arenaWallsTriggerProjRegCollisions) (_registeredCollisions p)

                projectiles'           = filter inTriggerProjRegCollisions projectiles
                collisionProjHashedIds = S.fromList $ catMaybes (map checkTriggerProjCollision projectiles')
            modify (roomArenaWallsTriggerPlayerAttackCollision collisionProjHashedIds arenaWalls ++)

    _ -> []
