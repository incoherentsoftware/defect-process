module Level.Types
    ( module Level.DangerValue
    , Level(..)
    ) where

import Level.DangerValue
import Level.Room.Chooser.Types
import Level.Room.Types

data Level = Level
    { _currentDangerValue :: DangerValue
    , _room               :: Room
    , _roomChooser        :: RoomChooser
    }
