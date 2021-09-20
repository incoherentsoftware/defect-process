module Level.Room.Chooser.Types
    ( RoomContentType(..)
    , RoomChooser(..)
    ) where

import qualified Data.Set as S

import Level.Room.Event.Types
import Level.Room.Types

data RoomContentType
    = HealthContentType
    | ShopContentType
    | GoldChunkContentType
    | EventContentType RoomEventType
    | NoContentType
    deriving (Eq, Show)

-- NOTE: this is modified from the full source, most of the room chooser logic is not included
data RoomChooser = RoomChooser
    { _currentRoomType       :: RoomType
    , _numArenas             :: Int
    , _visitedArenaRoomNames :: S.Set RoomName
    }
