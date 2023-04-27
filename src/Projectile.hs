module Projectile
    ( module P
    , projectileCollisionHitbox
    , mkProjectile
    , projectileHitbox
    , updateProjectileMsgs
    , sortProjectileCollisions
    ) where

import Data.Dynamic  (fromDynamic, toDyn)
import Data.Foldable (foldlM)
import Data.Typeable (Typeable)
import qualified Data.List as L
import qualified Data.Set  as S

import AppEnv
import Collision.Entity
import Collision.Hitbox
import Id
import Msg
import Projectile.Types as P
import Util

projectileCollisionHitbox :: ProjectileCollision -> Hitbox
projectileCollisionHitbox = \case
    ProjSurfaceCollision hbx _            -> hbx
    ProjPlayerCollision player            -> collisionEntityHitbox player
    ProjEnemyCollision enemy              -> collisionEntityHitbox enemy
    ProjRoomItemCollision (Some roomItem) -> collisionEntityHitbox roomItem
    ProjPlayerAttackCollision hbx _       -> hbx

mkProjectile :: Typeable d => d -> MsgId -> Hitbox -> Secs -> Projectile d
mkProjectile dat msgId hbx ttl = Projectile
    { _data                 = dat
    , _msgId                = msgId
    , _hitbox               = const hbx
    , _surface              = const Nothing
    , _vel                  = zeroVel2
    , _ttl                  = ttl
    , _ownerId              = NullId
    , _registeredCollisions = S.empty
    , _update               = return . id
    , _think                = const $ return []
    , _updateDynamic        = updateDynamic
    , _draw                 = const $ return ()
    , _processCollisions    = \_ _ -> []
    , _voluntaryClear       = const Nothing
    }

updateDynamic :: Typeable d => ProjectileUpdateDynamic d (AppEnv UpdateProjectileMsgsPhase)
updateDynamic dyn proj
    | Just update <- fromDynamic dyn  = return $ update proj
    | Just updateM <- fromDynamic dyn = updateM proj
    | otherwise                       = return proj

projectileHitbox :: Projectile d -> Hitbox
projectileHitbox proj = (P._hitbox proj) proj

updateProjectileMsgs :: Projectile d -> AppEnv UpdateProjectileMsgsPhase (Projectile d)
updateProjectileMsgs proj = foldlM processMsg proj =<< readMsgsTo (_msgId proj)
    where
        processMsg :: Projectile d -> ProjectileMsgPayload -> AppEnv UpdateProjectileMsgsPhase (Projectile d)
        processMsg !p d = case d of
            ProjectileMsgSetVelocity vel -> return $ p {_vel = vel}
            ProjectileMsgSetHitbox hbx   -> return $ p {_hitbox = const hbx}
            ProjectileMsgSetTtl ttl      -> return $ p {_ttl = ttl}
            ProjectileMsgVoluntaryClear  -> return p
            ProjectileMsgRemoveCollision -> return $ p {_registeredCollisions = S.empty}
            ProjectileMsgRemoveThink     -> return $ p {_think = const (return [])}
            ProjectileMsgRemoveUpdate    -> return $ p {_update = return . id}
            ProjectileMsgUpdate update   -> (_updateDynamic p) (toDyn update) p
            ProjectileMsgUpdateM update  -> (_updateDynamic p) (toDyn update) p

-- sorts by distance from projectile first vertex pos to collision intersect pos
sortProjectileCollisions :: [ProjectileCollision] -> Projectile d -> [(Pos2, ProjectileCollision)]
sortProjectileCollisions projCollisions proj =
    L.sortBy compareCollision [(collisionIntersectPos c, c) | c <- projCollisions]
        where
            projHbx = projectileHitbox proj

            compareCollision :: (Pos2, ProjectileCollision) -> (Pos2, ProjectileCollision) -> Ordering
            compareCollision (intersectPos1, _) (intersectPos2, _) =
                compare (vecDistSq startPos intersectPos1) (vecDistSq startPos intersectPos2)
                    where startPos = hitboxStartVertex projHbx

            collisionIntersectPos :: ProjectileCollision -> Pos2
            collisionIntersectPos collision = hitboxAvgIntersectPos projHbx collisionHbx
                where collisionHbx = projectileCollisionHitbox collision
