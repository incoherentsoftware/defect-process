module Level.Room.Trigger.Util
    ( removeTriggerMessage
    , updateTriggerThinkMessage
    ) where

import AppEnv
import Level.Room.Trigger
import Msg

removeTriggerMessage :: RoomTrigger -> Msg ThinkLevelMsgsPhase
removeTriggerMessage trigger = mkMsg $ RoomMsgRemoveTrigger triggerId
    where triggerId = _msgId (trigger :: RoomTrigger)

updateTriggerThinkMessage :: RoomTriggerThink (AppEnv ThinkLevelMsgsPhase) -> RoomTrigger -> Msg ThinkLevelMsgsPhase
updateTriggerThinkMessage think trigger = mkMsgTo (RoomMsgUpdateTrigger updateThink) triggerId
    where
        updateThink = \rt -> (rt :: RoomTrigger) {_think = think}
        triggerId   = _msgId (trigger :: RoomTrigger)
