module Player.Gun.MuzzleFlash.Types
    ( MuzzleFlashType(..)
    , MuzzleFlash(..)
    ) where

import qualified Data.List.NonEmpty as NE

import Util
import Window.Graphics

data MuzzleFlashType
    = LeadArmMuzzleFlash
    | RearArmMuzzleFlash

data MuzzleFlash = MuzzleFlash
    { _type    :: MuzzleFlashType
    , _offset  :: Pos2
    , _sprites :: NE.NonEmpty Sprite
    }
