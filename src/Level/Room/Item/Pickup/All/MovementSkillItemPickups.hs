module Level.Room.Item.Pickup.All.MovementSkillItemPickups
    ( module Player.MovementSkill.All
    , mkDashItemPickup
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.Level
import FileCache
import Level.Room.Item
import Level.Room.Item.Pickup
import Level.Room.Types
import Msg
import Player.MovementSkill.All
import Player.MovementSkill.Types
import Util
import Window.Graphics
import Window.InputState
--
-- NOTE: this is modified from the full source since only dash is included in this repo

dashPickupImgFileName = "dash-pickup.image" :: FileName

mkMovementSkillItemPickup :: MonadIO m => Pos2 -> ItemPickupData -> m (Some RoomItem)
mkMovementSkillItemPickup pos itemData = mkItemPickup pos MovementSkillPickupItemType itemData

mkDashItemPickup
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => RoomType
    -> Pos2
    -> m (Some RoomItem)
mkDashItemPickup roomType pos = do
    dashCost       <- readConfig _level _itemPickupMovementSkillGoldValue
    buyMsgPayload  <- PlayerMsgBuyMovementSkill <$> mkDashSkill <*> pure dashCost
    itemPickupData <- mkItemPickupData DashSkill buyMsgPayload dashCost dashPickupImgFileName roomType
    mkMovementSkillItemPickup pos itemPickupData
