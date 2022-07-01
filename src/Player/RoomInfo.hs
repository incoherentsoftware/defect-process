module Player.RoomInfo
    ( PlayerRoomInfo(..)
    , mkPlayerRoomInfo
    ) where

import Attack.Util
import Player.EquipmentInfo
import Player.Types

data PlayerRoomInfo = PlayerRoomInfo
    { _equipment :: PlayerEquipmentInfo
    , _health    :: Health
    }

mkPlayerRoomInfo :: Player -> PlayerRoomInfo
mkPlayerRoomInfo player = PlayerRoomInfo (mkPlayerEquipmentInfo player) health
    where health = _health (player :: Player)
