module Level.Room.Chooser.Types
    ( RoomContentType(..)
    , RoomChooser(..)
    ) where

import qualified Data.Set as S
import qualified Data.Vector as V

import Level.Room.Event.Types
import Level.Room.Types

data RoomContentType
    = HealthContentType
    | ShopContentType
    | GoldChunkContentType
    | EventContentType RoomEventType
    | NoContentType
    deriving (Eq, Show)

data RoomChooser = RoomChooser
    { _currentRoomType                :: RoomType
    , _arenaRoomTypes                 :: [RoomType]
    , _challengeRoomTypes             :: [RoomType]
    , _numArenas                      :: Int
    , _toTransitionRoomContentTypes   :: V.Vector RoomContentType
    , _fromTransitionRoomContentTypes :: V.Vector RoomContentType
    , _visitedArenaRoomNames          :: S.Set RoomName
    , _visitedToTransitionRoomNames   :: S.Set RoomName
    , _visitedFromTransitionRoomNames :: S.Set RoomName
    , _visitedChallengeRoomNames      :: S.Set RoomName
    , _roomEventTypeHistory           :: [RoomEventType]
    }
