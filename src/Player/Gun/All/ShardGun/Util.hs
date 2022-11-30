module Player.Gun.All.ShardGun.Util
    ( calculateShardImpalePos
    ) where

import Control.Monad.State (execState, get, modify, put, when)

import Collision
import Configs.All.PlayerGun.ShardGun
import Player.Gun.All.ShardGun.Data
import Projectile as P
import Util

calculateShardImpalePos :: CollisionEntity e => Pos2 -> e -> Projectile ShardGunData -> ShardGunConfig -> Pos2
calculateShardImpalePos intersectPos@(Pos2 intersectX intersectY) enemy shot cfg =
    let
        enemyHbx           = collisionEntityHitbox enemy
        shotHbx            = projectileHitbox shot
        shotStartPos       = hitboxStartVertex shotHbx
        centerPos          = hitboxCenter $ collisionEntityHitbox enemy
        angleVec           = vecNormalize $ intersectPos `vecSub` shotStartPos
        centerDist         = vecDist intersectPos centerPos
        impaleShardPercent = _impaleShardPercent cfg
        intersectOffset    = toPos2 $ angleVec `vecMul` (centerDist * impaleShardPercent)
    in flip execState intersectPos $ do
        modify (`vecAdd` intersectOffset)

        get >>= \(Pos2 x y) ->
            when (x > hitboxRight enemyHbx || x < hitboxLeft enemyHbx) $
                put $ Pos2 intersectX y

        get >>= \(Pos2 x y) ->
            when (y > hitboxBot enemyHbx || y < hitboxTop enemyHbx) $
                put $ Pos2 x intersectY
