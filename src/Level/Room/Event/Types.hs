module Level.Room.Event.Types
    ( RoomEventType(..)
    ) where

import Data.Aeson.Types (FromJSON)
import GHC.Generics     (Generic)

import Util

data RoomEventType
    = BouncingBallEvent
    | LightningStrikeEvent
    | SlotMachineEvent
    deriving (Bounded, Enum, Eq, FromJSON, Generic, Show)
    deriving anyclass PrettyShow
