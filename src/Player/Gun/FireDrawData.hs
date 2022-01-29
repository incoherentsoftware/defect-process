module Player.Gun.FireDrawData
    ( defaultGunFireDrawStateUncancelableSecs
    , GunFireDrawArmOrder(..)
    , GunFireDrawData(..)
    ) where

import qualified Data.Map as M

import Player.AimBody.Types
import Player.Gun.FireDrawAngle
import Player.Gun.FireDrawState.LegsState.Types
import Player.Gun.MuzzleFlash
import Util
import Window.Graphics

defaultGunFireDrawStateUncancelableSecs = 0.1 :: Secs

data GunFireDrawArmOrder
    = DrawLeadArmInFront
    | DrawRearArmInFront
    | DrawRearArmHeadInFront
    | DrawBothArmsInFront

data GunFireDrawData = GunFireDrawData
    { _fireDrawAngle           :: GunFireDrawAngle
    , _armOrders               :: M.Map GunFireDrawAngle GunFireDrawArmOrder
    , _headSprites             :: M.Map GunFireDrawAngle Sprite
    , _torsoSprites            :: M.Map GunFireDrawAngle Sprite
    , _leadArmSprites          :: M.Map GunFireDrawAngle Sprite
    , _rearArmSprites          :: M.Map GunFireDrawAngle Sprite
    , _legsSprites             :: Maybe LegsSprites
    , _muzzleFlash             :: Maybe MuzzleFlash
    , _calculatePlayerAimBody  :: CalculatePlayerAimBody
    , _uncancelableSecs        :: Secs
    }
