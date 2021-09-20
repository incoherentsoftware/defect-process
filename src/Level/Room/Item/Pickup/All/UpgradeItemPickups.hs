module Level.Room.Item.Pickup.All.UpgradeItemPickups
    ( mkDoubleJumpUpgradeItemPickup
    , mkMovementSkillUpgradeItemPickup
    , mkMeterUpgradeItemPickup
    ) where

import AppEnv
import Configs
import Configs.All.Level
import Level.Room.Item
import Level.Room.Item.Pickup
import Level.Room.Types
import Msg
import Player.Upgrade
import Util

doubleJumpImgFileName    = "double-jump-pickup.image"    :: FileName
movementSkillImgFileName = "movement-skill-pickup.image" :: FileName
meterImgFileName         = "meter-pickup.image"          :: FileName

mkDoubleJumpUpgradeItemPickup :: RoomType -> Pos2 -> AppEnv p (Some RoomItem)
mkDoubleJumpUpgradeItemPickup roomType pos = do
    doubleJumpCost   <- readConfig _level _itemPickupDoubleJumpUpgradeGoldValue
    let buyMsgPayload = PlayerMsgBuyUpgrade DoubleJumpUpgradeType doubleJumpCost
    itemData         <-
        mkItemPickupData DoubleJumpUpgradeType buyMsgPayload doubleJumpCost doubleJumpImgFileName roomType
    mkItemPickup pos UpgradePickupItemType itemData

mkMovementSkillUpgradeItemPickup :: RoomType -> Pos2 -> AppEnv p (Some RoomItem)
mkMovementSkillUpgradeItemPickup roomType pos = do
    movementSkillCost <- readConfig _level _itemPickupMovementSkillUpgradeGoldValue
    let buyMsgPayload  = PlayerMsgBuyUpgrade MovementSkillUpgradeType movementSkillCost
    itemData          <-
        mkItemPickupData MovementSkillUpgradeType buyMsgPayload movementSkillCost movementSkillImgFileName roomType
    mkItemPickup pos UpgradePickupItemType itemData

mkMeterUpgradeItemPickup :: RoomType -> Pos2 -> AppEnv p (Some RoomItem)
mkMeterUpgradeItemPickup roomType pos = do
    meterCost        <- readConfig _level _itemPickupMeterUpgradeGoldValue
    let buyMsgPayload = PlayerMsgBuyUpgrade MeterUpgradeType meterCost
    itemData         <- mkItemPickupData MeterUpgradeType buyMsgPayload meterCost meterImgFileName roomType
    mkItemPickup pos UpgradePickupItemType itemData
