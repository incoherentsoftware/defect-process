module Player.Gun.FireDrawState.Types
    ( GunFireDrawSprites(..)
    , GunFireDrawState(..)
    ) where

import Player.Gun.FireDrawData
import Player.Gun.FireDrawState.LegsState.Types
import Util
import Window.Graphics

data GunFireDrawSprites = GunFireDrawSprites
    { _head        :: Sprite
    , _torso       :: Sprite
    , _leadArm     :: Sprite
    , _rearArm     :: Sprite
    , _muzzleFlash :: Maybe Sprite
    }

data GunFireDrawState = GunFireDrawState
    { _activeSecs         :: Secs
    , _pos                :: Pos2
    , _dir                :: Direction
    , _aimAngle           :: Radians
    , _legsState          :: LegsState
    , _gunFireDrawSprites :: Maybe GunFireDrawSprites
    , _gunFireDrawData    :: Maybe GunFireDrawData
    , _legsSprites        :: LegsSprites
    }
