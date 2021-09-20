module Level.Room.Trigger
    ( module Level.Room.Trigger.Types
    , mkRoomTrigger
    ) where

import Control.Monad.IO.Class (MonadIO)

import Id
import Level.Room.Trigger.Types

mkRoomTrigger :: MonadIO m => m RoomTrigger
mkRoomTrigger = do
    triggerId <- newId
    return $ RoomTrigger
        { _msgId = triggerId
        , _think = \_ _ -> return []
        }
