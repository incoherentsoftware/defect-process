module Level.Room.Trigger.All.StartingShopItemPickupIndicator
    ( mkStartingShopItemPickupIndicator
    ) where

import Control.Monad.IO.Class (MonadIO)

import Level.Room
import Level.Room.Trigger
import Msg

mkStartingShopItemPickupIndicator :: MonadIO m => m RoomTrigger
mkStartingShopItemPickupIndicator  = do
    trigger <- mkRoomTrigger
    return $ (trigger :: RoomTrigger) {_think = think}

think :: Monad m => RoomTriggerThink m
think room trigger = return $ if
    | _type (room :: Room) /= startingShopRoomType -> [removeTriggerMsg]
    | isRoomPortalBarrierPlayerClose room          -> [mkMsg RoomMsgShowPickupItemIndicator, removeTriggerMsg]
    | otherwise                                    -> []
    where
        triggerId        = _msgId (trigger :: RoomTrigger)
        removeTriggerMsg = mkMsg $ RoomMsgRemoveTrigger triggerId
