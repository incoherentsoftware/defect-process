module Collision.Projectiles
    ( checkProjectilesCollisions
    ) where

import qualified Data.Set as S

import Collision
import Enemy
import Level.Room
import Level.Room.Item as RI
import Msg
import Player
import Projectile
import Util
import World.Surface as S

checkProjectilesCollisions :: Player -> [Some Enemy] -> Room -> [Some Projectile] -> [Msg ThinkCollisionMsgsPhase]
checkProjectilesCollisions player enemies room projectiles = foldr checkCollisions [] projectiles
    where
        checkCollisions :: Some Projectile -> [Msg ThinkCollisionMsgsPhase] -> [Msg ThinkCollisionMsgsPhase]
        checkCollisions (Some proj) !msgs = (_processCollisions proj) collisions proj ++ msgs
            where
                surfaceCollisions  = checkProjectileSurfaceCollisions proj (roomSurfaces room)
                enemyCollisions    = checkProjectileEnemyCollisions proj enemies
                playerCollisions   = checkProjectilePlayerCollisions proj player
                roomItemCollisions = checkProjectileRoomItemCollisions proj (_items room)
                collisions         = concat
                    [ surfaceCollisions
                    , enemyCollisions
                    , playerCollisions
                    , roomItemCollisions
                    ]

notRegistered :: ProjectileRegisteredCollision -> Projectile d -> Bool
notRegistered registeredCollision proj = registeredCollision `S.notMember` _registeredCollisions proj

registered :: ProjectileRegisteredCollision -> Projectile d -> Bool
registered registeredCollision proj = registeredCollision `S.member` _registeredCollisions proj

checkProjectileSurfaceCollisions :: Projectile d -> [Surface] -> [ProjectileCollision]
checkProjectileSurfaceCollisions proj surfaces
    | ProjRegisteredSurfaceCollision `notRegistered` proj = []
    | otherwise                                           =
        let projHbx = projectileHitbox proj
        in
            [ ProjSurfaceCollision (S._hitbox surface) (S._type surface)
            | surface <- filter (intersectsHitbox projHbx . S._hitbox) surfaces
            ]

checkProjectileEnemyCollisions :: Projectile d -> [Some Enemy] -> [ProjectileCollision]
checkProjectileEnemyCollisions proj enemies
    | ProjRegisteredEnemyCollision `registered` proj     = enemyCollisions enemies
    | ProjRegisteredEnemyRealCollision `registered` proj =
        let enemies' = filter (\(Some e) -> _dummyType e == EnemyRealType) enemies
        in enemyCollisions enemies'
    | otherwise                                          = []
    where
        enemyCollisions :: [Some Enemy] -> [ProjectileCollision]
        enemyCollisions es = [ProjEnemyCollision e | Some e <- collidingEs]
            where collidingEs = [Some e | Some e <- es, projectileHitbox proj `intersectsHitbox` enemyHitbox e]

checkProjectilePlayerCollisions :: Projectile d -> Player -> [ProjectileCollision]
checkProjectilePlayerCollisions proj player
    | ProjRegisteredPlayerCollision `notRegistered` proj = []
    | otherwise                                          = if
        | projectileHitbox proj `intersectsHitbox` playerHitbox player -> [ProjPlayerCollision player]
        | otherwise                                                    -> []

checkProjectileRoomItemCollisions :: Projectile d -> [Some RoomItem] -> [ProjectileCollision]
checkProjectileRoomItemCollisions proj roomItems
    | ProjRegisteredRoomItemCollision `notRegistered` proj = []
    | otherwise                                            =
        let
            collidingItems =
                [ Some ri
                | Some ri <- roomItems
                , _isAttackable ri
                , projectileHitbox proj `intersectsHitbox` RI._hitbox ri
                ]
        in [ProjRoomItemCollision (Some roomItem) | Some roomItem <- collidingItems]
