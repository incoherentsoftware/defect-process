module Level.Room.Trigger.All
    ( mkRoomTriggers
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Level.Room.Trigger
import Level.Room.Trigger.All.EndBoss
import Level.Room.Trigger.All.StartingShopItemPickupIndicator
import Level.Room.Trigger.All.StartingShopMoveControlsInfo
import Level.Room.Trigger.All.StartingShopRemoveItems
import Level.Room.Trigger.All.TutorialSetup
import Level.Room.Types
import Level.Room.Util

mkRoomTriggers :: (ConfigsRead m, MonadIO m) => RoomType -> m [RoomTrigger]
mkRoomTriggers roomType = sequenceA $ if
    | roomType == startingShopRoomType ->
        [ mkStartingShopItemPickupIndicator
        , mkStartingShopMoveControlsInfoTrigger
        , mkStartingShopRemoveItemsTrigger
        ]

    | roomType == tutorialRoomType -> [mkTutorialSetupTrigger]

    | roomType == endRoomType -> [mkEndBossTrigger]

    | otherwise -> []
