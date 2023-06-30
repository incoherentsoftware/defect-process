module Level.Room.Item
    ( module Level.Room.Item.Types
    , mkRoomItem
    ) where

import Data.Dynamic     (fromDynamic)
import Data.Typeable    (Typeable)
import qualified Data.Set as S

import Collision.Hitbox
import Level.Room.Item.Types
import Msg
import Util

mkRoomItem :: Typeable d => RoomItemType -> d -> MsgId -> Hitbox -> RoomItem d
mkRoomItem itemType itemData msgId hitbox = RoomItem
    { _type            = itemType
    , _data            = itemData
    , _msgId           = msgId
    , _hitbox          = hitbox
    , _vel             = zeroVel2
    , _isAttackable    = False
    , _hitByHashedIds  = S.empty
    , _think           = const $ return []
    , _update          = return . id
    , _updateDynamic   = updateDynamic
    , _draw            = const $ return ()
    , _playerCollision = \_ _ -> []
    , _inInteractRange = const False
    }

updateDynamic :: Typeable d => RoomItemUpdateDynamic d
updateDynamic dyn item = case fromDynamic dyn of
    Just update -> update item
    Nothing     -> item
