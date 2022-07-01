module InfoMsg.Util
    ( PlayerInfo(..)
    , RoomArenaWallsInfo(..)
    , playerInfoPos
    , playerInfoCenterPos
    ) where

import Collision.Hitbox
import Msg.Types
import Player.EquipmentInfo.Types
import Util

data PlayerInfo = PlayerInfo
    { _msgId            :: MsgId
    , _vel              :: Vel2
    , _dir              :: Direction
    , _hitbox           :: Hitbox
    , _groundBeneathPos :: Pos2
    , _touchingGround   :: Bool
    , _touchingWall     :: Bool
    , _equipment        :: PlayerEquipmentInfo
    }

data RoomArenaWallsInfo = RoomArenaWallsInfo
    { _leftWallPos  :: Pos2
    , _rightWallPos :: Pos2
    }

playerInfoPos :: PlayerInfo -> Pos2
playerInfoPos = hitboxBotCenter . _hitbox

playerInfoCenterPos :: PlayerInfo -> Pos2
playerInfoCenterPos = hitboxCenter . _hitbox
