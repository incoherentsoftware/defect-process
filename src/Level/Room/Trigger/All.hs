module Level.Room.Trigger.All
    ( mkAllRoomTriggers
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Level.Room.Trigger
import Level.Room.Trigger.All.EndBoss
import Level.Room.Trigger.All.StartingShopItemPickupIndicator
import Level.Room.Trigger.All.StartingShopMoveControlsInfo
import Level.Room.Trigger.All.StartingShopRemoveItems

mkAllRoomTriggers :: (ConfigsRead m, MonadIO m) => m [RoomTrigger]
mkAllRoomTriggers = sequenceA
    [ mkStartingShopItemPickupIndicator
    , mkStartingShopMoveControlsInfoTrigger
    , mkStartingShopRemoveItemsTrigger
    , mkEndBossTrigger
    ]
