module Player.Info
    ( PlayerInfo(..)
    , mkPlayerInfo
    ) where

import Attack.Util
import Player.EquipmentInfo
import Player.Types

data PlayerInfo = PlayerInfo
    { _equipment :: PlayerEquipmentInfo
    , _health    :: Health
    }

mkPlayerInfo :: Player -> PlayerInfo
mkPlayerInfo player = PlayerInfo (mkPlayerEquipmentInfo player) health
    where health = _health (player :: Player)
