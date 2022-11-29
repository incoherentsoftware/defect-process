module Particle.All.AttackSpecks.Types
    ( AttackSpecksType(..)
    , AttackSpecksPosition(..)
    , AttackSpecksDirection(..)
    ) where

import Data.Aeson.Types (FromJSON)
import GHC.Generics     (Generic)

data AttackSpecksType
    = SwordSpecksType
    | StaffSpecksType
    | GauntletsSpecksType
    | ScytheSpecksType
    | SpiritBladeSpecksType
    | BulletSpecksType
    | ShardSpecksType
    | ShardExplodeSpecksType
    | SpikeSpecksType
    | RicochetSpecksType
    | GrenadeSpecksType
    | MineSpecksType
    | GoldSpecksType
    | GrappleSpecksType
    deriving (FromJSON, Generic)

data AttackSpecksPosition
    = SpecksAtkIntersectPos
    | SpecksAtkCenterPos
    deriving (FromJSON, Generic)

data AttackSpecksDirection
    = SpecksLeftDir
    | SpecksRightDir
    | SpecksUpDir
    | SpecksAnyDir
    deriving (FromJSON, Generic)
