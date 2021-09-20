module Level.Room.Item.Pickup.Types
    ( ItemPickupStatus(..)
    , ItemPickupData(..)
    ) where

import qualified Data.Text as T

import Level.Room.Types
import Msg.Payload
import Window.Graphics
import World.Util

data ItemPickupStatus
    = ItemPickupNormalStatus
    | ItemPickupIndicatorStatus Sprite
    | ItemPickupReappearStatus Sprite

data ItemPickupData = ItemPickupData
    { _name                    :: T.Text
    , _buyMsgPayload           :: PlayerMsgPayload
    , _cost                    :: GoldValue
    , _touchingPlayer          :: Bool
    , _roomType                :: RoomType
    , _image                   :: Image
    , _costInputDisplayText    :: InputDisplayText
    , _buyInfoInputDisplayText :: InputDisplayText
    , _replaceDisplayText      :: DisplayText
    , _status                  :: ItemPickupStatus
    }
