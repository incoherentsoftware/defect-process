module Level.Room.Trigger.Types
    ( RoomTriggerThink
    , RoomTrigger(..)
    ) where

import AppEnv
import {-# SOURCE #-} Level.Room.Types
import {-# SOURCE #-} Msg.Types

type RoomTriggerThink m = Room -> RoomTrigger -> m [Msg ThinkLevelMsgsPhase]

data RoomTrigger = RoomTrigger
    { _msgId :: MsgId
    , _think :: RoomTriggerThink (AppEnv ThinkLevelMsgsPhase)
    }
