module Level.Room.Trigger.All.StartingShopMoveControlsInfo
    ( mkStartingShopMoveControlsInfoTrigger
    ) where

import Control.Monad.IO.Class (MonadIO)

import Constants
import Level.Room
import Level.Room.Trigger
import Msg
import Util
import Window.InputState

noMovementSecs = 5.0 :: Secs

mkStartingShopMoveControlsInfoTrigger :: MonadIO m => m RoomTrigger
mkStartingShopMoveControlsInfoTrigger  = do
    trigger <- mkRoomTrigger
    return $ (trigger :: RoomTrigger) {_think = think noMovementSecs}

think :: InputRead m => Secs -> RoomTriggerThink m
think noMovementTtl room trigger
    | _type room /= startingShopRoomType = return [removeTriggerMsg]
    | otherwise                          =
        let
            checkForInput :: InputState -> [Msg ThinkLevelMsgsPhase]
            checkForInput inputState
                | LeftAlias `aliasHold` inputState || RightAlias `aliasHold` inputState = [removeTriggerMsg]
                | otherwise                                                             = if
                    | noMovementTtl <= 0.0 -> [mkMsg UiMsgShowMoveControls]
                    | otherwise            ->
                        let
                            noMovementTtl' = noMovementTtl - timeStep
                            updateThink    = \rt -> (rt :: RoomTrigger) {_think = think noMovementTtl'}
                        in [mkMsgTo (RoomMsgUpdateTrigger updateThink) triggerId]
        in checkForInput <$> readInputState
    where
        triggerId        = _msgId (trigger :: RoomTrigger)
        removeTriggerMsg = mkMsg $ RoomMsgRemoveTrigger triggerId
