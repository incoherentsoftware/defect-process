module Level.Room.Item.Pickup.All.WeaponItemPickups
    ( mkSwordItemPickup
    ) where

import Control.Monad.IO.Class (MonadIO)

import AppEnv
import Configs
import Configs.All.Level
import Level.Room.Item
import Level.Room.Item.Pickup
import Level.Room.Types
import Msg
import Player.Weapon as W
import Player.Weapon.All
import Util

-- NOTE: this is modified from the full source since only sword is included in this repo

swordPickupImgFileName = "sword-pickup.image" :: FileName

mkWeaponItemPickup :: MonadIO m => Pos2 -> ItemPickupData -> m (Some RoomItem)
mkWeaponItemPickup pos itemData = mkItemPickup pos WeaponPickupItemType itemData

mkSwordItemPickup :: RoomType -> Pos2 -> AppEnv p (Some RoomItem)
mkSwordItemPickup roomType pos = do
    swordCost      <- readConfig _level _itemPickupWeaponGoldValue
    buyMsgPayload  <- PlayerMsgBuyWeapon <$> mkSwordWeapon <*> pure swordCost
    itemPickupData <- mkItemPickupData SwordWeapon buyMsgPayload swordCost swordPickupImgFileName roomType
    mkWeaponItemPickup pos itemPickupData
