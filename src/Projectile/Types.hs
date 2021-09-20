module Projectile.Types
    ( ProjectileRegisteredCollision(..)
    , ProjectileCollision(..)
    , ProjectileUpdate
    , ProjectileThink
    , ProjectileHitbox
    , ProjectileSurface
    , ProjectileUpdateDynamic
    , ProjectileDraw
    , ProjectileProcessCollisions
    , Projectile(..)
    ) where

import Data.Dynamic (Dynamic)
import qualified Data.Set as S

import AppEnv.Types
import Attack.Types
import Collision
import Enemy.Types
import Level.Room.Item.Types
import Msg.Types
import Util
import World.Surface.Types

data ProjectileRegisteredCollision
    = ProjRegisteredSurfaceCollision
    | ProjRegisteredPlayerCollision
    | ProjRegisteredEnemyCollision
    | ProjRegisteredEnemyRealCollision  -- only EnemyRealType collisions, ignore EnemyDummyType
    | ProjRegisteredRoomItemCollision
    | ProjRegisteredPlayerAttackCollision
    deriving (Eq, Ord)

data ProjectileCollision where
    ProjSurfaceCollision      :: Hitbox -> SurfaceType -> ProjectileCollision
    ProjPlayerCollision       :: CollisionEntity e => e -> ProjectileCollision
    ProjEnemyCollision        :: Enemy d -> ProjectileCollision
    ProjRoomItemCollision     :: Some RoomItem -> ProjectileCollision
    ProjPlayerAttackCollision :: Hitbox -> Attack -> ProjectileCollision

type ProjectileUpdate d m          = Projectile d -> m (Projectile d)
type ProjectileThink d m           = Projectile d -> m [Msg ThinkProjectileMsgsPhase]
type ProjectileHitbox d            = Projectile d -> Hitbox
type ProjectileSurface d           = Projectile d -> Maybe Surface
type ProjectileUpdateDynamic d m   = Dynamic -> Projectile d -> m (Projectile d)
type ProjectileDraw d m            = Projectile d -> m ()
type ProjectileProcessCollisions d = [ProjectileCollision] -> Projectile d -> [Msg ThinkCollisionMsgsPhase]

data Projectile d = Projectile
    { _data                 :: d
    , _msgId                :: MsgId
    , _vel                  :: Vel2
    , _ttl                  :: Secs
    , _ownerId              :: MsgId
    , _registeredCollisions :: S.Set ProjectileRegisteredCollision
    , _hitbox               :: ProjectileHitbox d
    , _surface              :: ProjectileSurface d
    , _think                :: ProjectileThink d (AppEnv ThinkProjectileMsgsPhase)
    , _update               :: ProjectileUpdate d (AppEnv UpdateProjectileMsgsPhase)
    , _updateDynamic        :: ProjectileUpdateDynamic d (AppEnv UpdateProjectileMsgsPhase)
    , _draw                 :: ProjectileDraw d (AppEnv DrawMsgsPhase)
    , _processCollisions    :: ProjectileProcessCollisions d
    }

instance Eq (Projectile d) where
    (==) :: Projectile d -> Projectile d -> Bool
    (==) p1 p2 = _msgId (p1 :: Projectile d) == _msgId (p2 :: Projectile d)

instance Ord (Projectile d) where
    (<=) :: Projectile d -> Projectile d -> Bool
    (<=) p1 p2 = _msgId (p1 :: Projectile d) <= _msgId (p2 :: Projectile d)
