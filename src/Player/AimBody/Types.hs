module Player.AimBody.Types
    ( CalculatePlayerAimBody
    , PlayerAimBody(..)
    ) where

import Player.Gun.FireDrawAngle
import Player.Gun.FireDrawState.LegsState.Types
import Util

type CalculatePlayerAimBody = GunFireDrawAngle -> Pos2 -> Radians -> Direction -> LegsState -> PlayerAimBody

data PlayerAimBody = PlayerAimBody
    { _headAngle       :: Radians
    , _neckPos         :: Pos2
    , _torsoAngle      :: Radians
    , _leadShoulderPos :: Pos2
    , _rearShoulderPos :: Pos2
    , _hipsPos         :: Pos2
    , _leadArmAngle    :: Radians
    , _rearArmAngle    :: Radians
    , _aimDir          :: Direction
    }
