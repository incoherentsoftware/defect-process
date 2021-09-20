module Level.Room.Item.Pickup.JSON
    ( RoomItemPickupType(..)
    , RoomItemPickupJSON(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util

data RoomItemPickupType
    = RandomWeaponPickup
    | RandomGunPickup
    | RandomMovementSkillPickup
    | RandomSecondarySkillPickup
    | RandomUpgradePickup
    | RandomAnyPickup
    | HealthPickup
    deriving Generic
    deriving anyclass FromJSON

data RoomItemPickupJSON = RoomItemPickupJSON
    { _type :: RoomItemPickupType
    , _pos  :: Pos2
    }
    deriving Generic

instance FromJSON RoomItemPickupJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
