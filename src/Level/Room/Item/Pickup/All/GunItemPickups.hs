module Level.Room.Item.Pickup.All.GunItemPickups
    ( module Player.Gun.All
    , mkRevolverItemPickup
    ) where

import Control.Monad.IO.Class (MonadIO)

import AppEnv
import Configs
import Configs.All.Level
import Level.Room.Item
import Level.Room.Item.Pickup
import Level.Room.Types
import Msg
import Player.Gun.All
import Player.Gun.Types
import Util

-- NOTE: this is modified from the full source since only revolver is included in this repo

revolverPickupImgFileName = "revolver-pickup.image" :: FileName

mkGunItemPickup :: MonadIO m => Pos2 -> ItemPickupData -> m (Some RoomItem)
mkGunItemPickup pos itemData = mkItemPickup pos GunPickupItemType itemData

mkRevolverItemPickup :: RoomType -> Pos2 -> AppEnv p (Some RoomItem)
mkRevolverItemPickup roomType pos = do
    revolverCost   <- readConfig _level _itemPickupGunGoldValue
    buyMsgPayload  <- PlayerMsgBuyGun <$> mkRevolverGun <*> pure revolverCost
    itemPickupData <- mkItemPickupData RevolverGun buyMsgPayload revolverCost revolverPickupImgFileName roomType
    mkGunItemPickup pos itemPickupData
