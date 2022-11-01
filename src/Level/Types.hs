module Level.Types
    ( module Level.DangerValue
    , Level(..)
    ) where

import qualified Data.Map as M

import Level.DangerValue
import Level.Room.Chooser.Types
import Level.Room.Item.Types
import Level.Room.Types
import Util

data Level = Level
    { _currentDangerValue :: DangerValue
    , _room              :: Room
    , _roomChooser       :: RoomChooser
    , _roomItemsOverride :: M.Map RoomType [Some RoomItem]
    }
