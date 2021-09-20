module Collision.Entity
    ( CollisionEntity(..)
    , collisionEntityNotPrevHitBy
    ) where

import qualified Data.Set as S

import Collision.Hitbox.Types
import Id
import Msg.Types
import Util

class CollisionEntity e where
    collisionEntityMsgId          :: e -> MsgId
    collisionEntityHitbox         :: e -> Hitbox
    collisionEntityPrevHitbox     :: e -> Hitbox
    collisionEntityVel            :: e -> Vel2
    collisionEntityDir            :: e -> Direction
    collisionEntityHitByHashedIds :: e -> S.Set HashedId

    collisionEntityWallProximityHitbox :: e -> Maybe Hitbox
    collisionEntityWallProximityHitbox _ = Nothing

collisionEntityNotPrevHitBy :: CollisionEntity e => HashedId -> e -> Bool
collisionEntityNotPrevHitBy hitHashedId entity = hitHashedId `notElem` collisionEntityHitByHashedIds entity
