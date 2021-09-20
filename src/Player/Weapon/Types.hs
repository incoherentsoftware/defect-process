module Player.Weapon.Types
    ( WeaponType(..)
    , WeaponAttackStatus(..)
    , WeaponThinkStatus(..)
    , WeaponUpdateStatus(..)
    , WeaponThink
    , WeaponUpdate
    , WeaponProcessDynamic
    , WeaponDrawOverlay
    , Weapon(..)
    ) where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Dynamic     (Dynamic)
import GHC.Generics     (Generic)
import qualified Data.Text as T

import AppEnv.Types
import Attack.Types
import Msg.Types
import Util
import {-# SOURCE #-} Player.Types

data WeaponType
    = SwordWeapon
    | GauntletsWeapon
    | ScytheWeapon
    | StaffWeapon
    deriving (Bounded, Enum, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance PrettyShow WeaponType where
    prettyShow :: WeaponType -> T.Text
    prettyShow = \case
        SwordWeapon     -> "Sword"
        GauntletsWeapon -> "Explosive Fist"
        ScytheWeapon    -> "Scythe"
        StaffWeapon     -> "Staff"

data WeaponAttackStatus
    = WeaponAttackReady
    | WeaponAttackNotReady
    deriving Eq

data WeaponThinkStatus
    = WeaponThinkForeground WeaponAttackStatus
    | WeaponThinkBackground
    deriving Eq

data WeaponUpdateStatus
    = WeaponUpdateForeground
    | WeaponUpdateBackground
    deriving Eq

type WeaponThink d m        = WeaponThinkStatus -> Player -> Maybe Attack -> Weapon d -> m [Msg ThinkPlayerMsgsPhase]
type WeaponUpdate d m       = WeaponUpdateStatus -> Player -> Maybe Attack -> Weapon d -> m (Weapon d)
type WeaponProcessDynamic d = Dynamic -> Weapon d -> Weapon d
type WeaponDrawOverlay d m  = Player -> Maybe Attack -> Weapon d -> m ()

data Weapon d = Weapon
    { _data             :: d
    , _type             :: WeaponType
    , _think            :: WeaponThink d (AppEnv ThinkPlayerMsgsPhase)
    , _update           :: WeaponUpdate d (AppEnv UpdatePlayerMsgsPhase)
    , _processDynamic   :: WeaponProcessDynamic d
    , _drawOverlay      :: WeaponDrawOverlay d (AppEnv DrawMsgsPhase)
    , _hitSoundFilePath :: FilePath
    }
