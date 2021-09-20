module Collision.Manager
    ( checkCollisions
    ) where

import Data.Foldable (traverse_)

import Collision.Level
import Collision.OutOfBounds
import Collision.PlayerEnemies
import Collision.PlayerEnemiesAttacks
import Collision.Projectiles
import Collision.Surfaces
import Enemy
import Level.Room
import Msg
import Player
import Projectile.Manager
import Util

checkCollisions :: MsgsWrite ThinkCollisionMsgsPhase m => Player -> [Some Enemy] -> Room -> ProjectileManager -> m ()
checkCollisions player enemies room projectileManager = traverse_ writeMsgs allMsgs
    where
        lvlSurfaces = roomSurfaces room
        surfaces    = lvlSurfaces ++ projectileManagerProjectileSurfaces projectileManager

        -- player/enemies w/ surfaces
        playerSurfaceMsgs = checkPlayerSurfaceCollisions player surfaces
        enemySurfaceMsgs  = checkEnemySurfaceCollisions enemies lvlSurfaces
        roomItems         = _items room
        itemSurfaceMsgs   = checkRoomItemSurfaceCollisions roomItems lvlSurfaces
        surfaceMsgs       = playerSurfaceMsgs ++ enemySurfaceMsgs ++ itemSurfaceMsgs

        -- player w/ enemies
        playerEnemyMsgs = checkPlayerEnemyCollisions player enemies

        -- player attacks w/ enemy attacks
        playerAttackMsgs        = checkPlayerAttackEnemiesCollisions player enemies
        enemyAttackMsgs         = checkPlayerEnemyAttacksCollisions player enemies
        playerAttackSurfaceMsgs = checkPlayerAttackSurfaceCollisions player surfaces
        attackMsgs              = playerAttackMsgs ++ enemyAttackMsgs ++ playerAttackSurfaceMsgs

        -- projectiles w/ player/enemies/room
        projectiles    = _projectiles projectileManager
        projectileMsgs = checkProjectilesCollisions player enemies room projectiles

        -- player w/ portals/room items/spring launchers
        portalMsgs         = checkRoomPortalPlayerCollisions room player
        roomItemsMsgs      = checkRoomItemCollisions roomItems player
        springLauncherMsgs = checkRoomSpringLauncherCollisions room player

        -- player/player attacks/projectiles w/ room arena walls
        arenaWallsMsgs = checkRoomArenaWallsCollisions room projectiles player

        -- player/enemies out of bounds
        outOfBoundsMsgs = checkPlayerOutOfBounds player room ++ checkEnemyOutOfBounds enemies room

        allMsgs =
            [ surfaceMsgs
            , playerEnemyMsgs
            , attackMsgs
            , projectileMsgs
            , portalMsgs
            , roomItemsMsgs
            , springLauncherMsgs
            , arenaWallsMsgs
            , outOfBoundsMsgs
            ]
