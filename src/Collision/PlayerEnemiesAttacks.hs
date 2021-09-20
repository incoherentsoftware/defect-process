module Collision.PlayerEnemiesAttacks
    ( checkPlayerAttackEnemiesCollisions
    , checkPlayerEnemyAttacksCollisions
    , checkPlayerAttackSurfaceCollisions
    ) where

import Control.Monad (guard)
import Data.Maybe    (fromMaybe)

import Attack
import Collision
import Enemy as E
import Msg
import Player
import Util
import World.Surface

-- collisions between player's attack and enemies
checkPlayerAttackEnemiesCollisions :: Player -> [Some Enemy] -> [Msg ThinkCollisionMsgsPhase]
checkPlayerAttackEnemiesCollisions player enemies = fromMaybe [] $ do
    atk       <- _attack (player :: Player)
    atkHitbox <- attackHitbox atk

    let
        checkCollision :: Some Enemy -> [Msg ThinkCollisionMsgsPhase] -> [Msg ThinkCollisionMsgsPhase]
        checkCollision (Some e) msgs
            | enemyHitbox e `intersectsHitbox` atkHitbox = attackEnemyHitMessages e atk ++ msgs
            | otherwise                                  = msgs

    Just $ foldr checkCollision [] enemies

-- collisions between enemy attacks and player
checkPlayerEnemyAttacksCollisions :: Player -> [Some Enemy] -> [Msg ThinkCollisionMsgsPhase]
checkPlayerEnemyAttacksCollisions player enemies = foldr checkCollision [] enemies
    where
        checkCollision :: Some Enemy -> [Msg ThinkCollisionMsgsPhase] -> [Msg ThinkCollisionMsgsPhase]
        checkCollision (Some enemy) msgs = maybe msgs (++ msgs) $ do
            atk       <- E._attack enemy
            atkHitbox <- attackHitbox atk
            guard $ playerHitbox player `intersectsHitbox` atkHitbox

            Just $ attackCollisionEntityHitMessages player atk

-- collisions between player's attack and surfaces
checkPlayerAttackSurfaceCollisions :: Player -> [Surface] -> [Msg ThinkCollisionMsgsPhase]
checkPlayerAttackSurfaceCollisions player surfaces = fromMaybe [] $ do
    atk       <- _attack (player :: Player)
    atkHitbox <- attackHitbox atk

    let
        checkCollision :: Surface -> [Msg ThinkCollisionMsgsPhase] -> [Msg ThinkCollisionMsgsPhase]
        checkCollision s msgs
            | hbx `intersectsHitbox` atkHitbox = attackSurfaceHitMessages hbx atk ++ msgs
            | otherwise                        = msgs
            where hbx = _hitbox (s :: Surface)

    Just $ foldr checkCollision [] surfaces
