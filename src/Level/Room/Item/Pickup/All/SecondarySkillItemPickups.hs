module Level.Room.Item.Pickup.All.SecondarySkillItemPickups
    ( mkStoneFormItemPickup
    ) where

import AppEnv
import Configs
import Configs.All.Level
import Level.Room.Item
import Level.Room.Item.Pickup
import Level.Room.Types
import Msg
import Player.SecondarySkill.All
import Player.SecondarySkill.Types
import Util

-- NOTE: this is modified from the full source since only stoneForm is included in this repo

stoneFormImgFileName = "stone-form-pickup.image" :: FileName

mkStoneFormItemPickup :: RoomType -> Pos2 -> AppEnv p (Some RoomItem)
mkStoneFormItemPickup roomType pos = do
    stoneFormCost <- readConfig _level _itemPickupStoneFormSkillGoldValue
    buyMsgPayload <- PlayerMsgBuySecondarySkill <$> mkStoneFormSkill <*> pure stoneFormCost
    itemData      <- mkItemPickupData StoneFormSkill buyMsgPayload stoneFormCost stoneFormImgFileName roomType
    mkItemPickup pos SecondarySkillPickupItemType itemData
