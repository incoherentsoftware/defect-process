module Collision.PlayerEnemies
    ( checkPlayerEnemyCollisions
    ) where

import Collision
import Constants
import Enemy as E
import Msg
import Player
import Util

groundPushbackVelMultiplier        = 40.0 :: Float
airRisingPushbackAirVelMultiplier  = 20.0 :: Float
airFallingPushbackAirVelMultiplier = 0.0  :: Float
maxOffsetX                         = 5.0 :: Float

checkPlayerEnemyCollisions :: Player -> [Some Enemy] -> [Msg ThinkCollisionMsgsPhase]
checkPlayerEnemyCollisions player enemies = foldr checkCollision [] enemies'
    where
        checkCollision :: Some Enemy -> [Msg ThinkCollisionMsgsPhase] -> [Msg ThinkCollisionMsgsPhase]
        checkCollision (Some enemy) msgs
            | enemyOnGround && playerHbx `intersectsHitbox` enemyHbx =
                let
                    leftDist  = abs $ hitboxRight playerHbx - hitboxLeft enemyHbx
                    rightDist = abs $ hitboxRight enemyHbx - hitboxLeft playerHbx
                in if
                    | leftDist < rightDist ->
                        let
                            velX     = -leftDist * pushbackVelMultiplier
                            offsetX  = velX * timeStep
                            offsetX' = if offsetX < (-maxOffsetX) then -maxOffsetX else offsetX
                        in mkMsgEx (PlayerMsgPushbackOffset offsetX') MsgEndOrder:msgs

                    | otherwise ->
                        let
                            velX     = rightDist * pushbackVelMultiplier
                            offsetX  = velX * timeStep
                            offsetX' = if offsetX > maxOffsetX then maxOffsetX else offsetX
                        in mkMsgEx (PlayerMsgPushbackOffset offsetX') MsgEndOrder:msgs

            | otherwise = msgs

            where
                enemyOnGround = _touchingGround (E._flags enemy :: EnemyFlags)
                playerHbx     = playerHitbox player
                enemyHbx      = enemyHitbox enemy

                playerOnGround          = _touchingGround (_flags (player :: Player) :: PlayerFlags)
                playerVelY              = vecY $ _vel (player :: Player)
                pushbackVelMultiplier
                    | playerOnGround    = groundPushbackVelMultiplier
                    | playerVelY <= 0.0 = airRisingPushbackAirVelMultiplier
                    | otherwise         = airFallingPushbackAirVelMultiplier

        enemies' = filter (\(Some e) -> _dummyType e == EnemyRealType) enemies
