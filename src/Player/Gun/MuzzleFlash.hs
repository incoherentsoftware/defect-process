module Player.Gun.MuzzleFlash
    ( module Player.Gun.MuzzleFlash.Types
    , mkMuzzleFlash
    ) where

import qualified Data.List.NonEmpty as NE

import Player.Gun.MuzzleFlash.Types
import Util
import Window.Graphics

mkMuzzleFlash :: MuzzleFlashType -> Pos2 -> NE.NonEmpty Sprite -> MuzzleFlash
mkMuzzleFlash muzzleFlashType offset sprites = MuzzleFlash
    { _type    = muzzleFlashType
    , _offset  = offset
    , _sprites = sprites
    } :: MuzzleFlash
