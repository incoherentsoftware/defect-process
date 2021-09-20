module Level.Room.Item.Pickup.All.HealthItemPickup
    ( mkHealthItemPickup
    ) where

import qualified Data.Text as T

import AppEnv
import Configs
import Configs.All.Level
import Level.Room.Item
import Level.Room.Item.Pickup
import Level.Room.Types
import Msg
import Stats.Manager
import Util
import World.Util

healthPickupImgFileName = "health-pickup.image" :: FileName

data HealthPickup = HealthPickup
    deriving Show

instance PrettyShow HealthPickup where
    prettyShow :: HealthPickup -> T.Text
    prettyShow _ = "Health"

mkHealthItemPickup :: RoomType -> Pos2 -> StatsManager -> AppEnv p (Some RoomItem)
mkHealthItemPickup roomType pos statsManager = do
    cfg <- _level <$> readConfigs
    let
        healthBaseCost         = _itemPickupHealthGoldValue cfg
        healthMultiplicandCost = _itemPickupHealthMultiplicandGoldValue cfg
        numBoughtHealth        = _numBoughtHealth statsManager
        healthCost             = healthBaseCost + GoldValue (_int healthMultiplicandCost * numBoughtHealth)
        buyMsgPayload          = PlayerMsgBuyHealth healthCost

    itemPickupData <- mkItemPickupData HealthPickup buyMsgPayload healthCost healthPickupImgFileName roomType
    mkItemPickup pos HealthPickupItemType itemPickupData
