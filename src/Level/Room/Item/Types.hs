module Level.Room.Item.Types
    ( RoomItemType(..)
    , RoomItemThink
    , RoomItemUpdate
    , RoomItemUpdateDynamic
    , RoomItemDraw
    , RoomItemPlayerCollision
    , RoomItemInInteractRange
    , RoomItemInit
    , RoomItem(..)
    ) where

import Data.Dynamic (Dynamic)
import qualified Data.Set as S

import AppEnv.Types
import Collision
import Id
import Player.Types
import Util
import {-# SOURCE #-} Msg.Types

data RoomItemType
    = GoldChunkItemType
    | WeaponPickupItemType
    | GunPickupItemType
    | MovementSkillPickupItemType
    | SecondarySkillPickupItemType
    | UpgradePickupItemType
    | HealthPickupItemType
    | InfoSignItemType
    | TutorialSignItemType
    | RefreshStationItemType
    | JukeboxItemType
    | EventActivatorItemType
    deriving (Eq, Ord, Show)

type RoomItemThink d m         = RoomItem d -> m [Msg ThinkLevelMsgsPhase]
type RoomItemUpdate d m        = RoomItem d -> m (RoomItem d)
type RoomItemUpdateDynamic d   = Dynamic -> RoomItem d -> RoomItem d
type RoomItemDraw d m          = RoomItem d -> m ()
type RoomItemPlayerCollision d = Player -> RoomItem d -> [Msg ThinkCollisionMsgsPhase]
type RoomItemInInteractRange d = RoomItem d -> Bool

type RoomItemInit p = Pos2 -> AppEnv p (Some RoomItem)

data RoomItem d = RoomItem
    { _type            :: RoomItemType
    , _data            :: d
    , _msgId           :: MsgId
    , _hitbox          :: Hitbox
    , _vel             :: Vel2
    , _isAttackable    :: Bool
    , _hitByHashedIds  :: S.Set HashedId
    , _think           :: RoomItemThink d (AppEnv ThinkLevelMsgsPhase)
    , _update          :: RoomItemUpdate d (AppEnv UpdateLevelMsgsPhase)
    , _updateDynamic   :: RoomItemUpdateDynamic d
    , _draw            :: RoomItemDraw d (AppEnv DrawMsgsPhase)
    , _playerCollision :: RoomItemPlayerCollision d
    , _inInteractRange :: RoomItemInInteractRange d
    }

instance CollisionEntity (RoomItem d) where
    collisionEntityMsgId :: RoomItem d -> MsgId
    collisionEntityMsgId = _msgId

    collisionEntityHitbox :: RoomItem d -> Hitbox
    collisionEntityHitbox = _hitbox

    collisionEntityPrevHitbox :: RoomItem d -> Hitbox
    collisionEntityPrevHitbox = collisionEntityHitbox  -- TODO

    collisionEntityVel :: RoomItem d -> Vel2
    collisionEntityVel = _vel

    collisionEntityDir :: RoomItem d -> Direction
    collisionEntityDir _ = RightDir

    collisionEntityHitByHashedIds :: RoomItem d -> S.Set HashedId
    collisionEntityHitByHashedIds = _hitByHashedIds
