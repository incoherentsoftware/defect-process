module Level.Room.Types
    ( RoomName
    , RoomType(..)
    , Room
    ) where

import qualified Data.Text as T

type RoomName = T.Text

data RoomType
    = EmptyRoomType
    | NextRoomType
    | ArenaRoomType RoomName
    | FromTransitionRoomType RoomName
    | ToTransitionRoomType RoomName
    | ChallengeRoomType RoomName
    | SpecialRoomType RoomName

data Room
