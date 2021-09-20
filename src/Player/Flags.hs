module Player.Flags
    ( PlayerFlags(..)
    , PlayerRisingJumpFlag(..)
    , PlayerWallProximityFlag(..)
    , mkPlayerFlags
    , updatePlayerFlags
    ) where

import Util

data PlayerRisingJumpFlag
    = PlayerNotRisingJumpFlag
    | PlayerRisingFullJumpFlag Secs
    | PlayerRisingShortJumpFlag
    deriving Eq

data PlayerWallProximityFlag
    = PlayerWallProximityLeft OffsetX
    | PlayerWallProximityRight OffsetX
    | PlayerWallProximityNone
    deriving Show

data PlayerFlags = PlayerFlags
    { _movingLeftRight       :: Bool
    , _touchingGround        :: Bool
    , _touchingWall          :: Bool
    , _touchingWallNearTop   :: Bool
    , _touchingLeftWall      :: Bool
    , _touchingRightWall     :: Bool
    , _touchingRoof          :: Bool
    , _touchingPlatform      :: Bool
    , _onSpeedRail           :: Bool
    , _onMovingLeftPlatform  :: Bool
    , _onMovingRightPlatform :: Bool
    , _jumped                :: Bool
    , _doubleJumped          :: Bool
    , _walkCanceled          :: Bool
    , _onPlatform            :: Bool
    , _platformDropped       :: Bool
    , _platformDropping      :: Bool
    , _attacked              :: Bool
    , _firedGun              :: Bool
    , _movementSkilled       :: Bool
    , _gettingHit            :: Bool
    , _phased                :: Bool
    , _willFallOffGround     :: Bool
    , _warpingOut            :: Bool
    , _touchingInfoSign      :: Bool
    , _risingJump            :: PlayerRisingJumpFlag
    , _wallProximity         :: PlayerWallProximityFlag
    }

mkPlayerFlags :: PlayerFlags
mkPlayerFlags = PlayerFlags
    { _movingLeftRight       = False
    , _touchingGround        = False
    , _touchingWall          = False
    , _touchingWallNearTop   = False
    , _touchingLeftWall      = False
    , _touchingRightWall     = False
    , _touchingRoof          = False
    , _touchingPlatform      = False
    , _onSpeedRail           = False
    , _onMovingLeftPlatform  = False
    , _onMovingRightPlatform = False
    , _jumped                = False
    , _doubleJumped          = False
    , _walkCanceled          = False
    , _onPlatform            = False
    , _platformDropped       = False
    , _platformDropping      = False
    , _attacked              = False
    , _firedGun              = False
    , _movementSkilled       = False
    , _gettingHit            = False
    , _phased                = False
    , _willFallOffGround     = False
    , _warpingOut            = False
    , _touchingInfoSign      = False
    , _risingJump            = PlayerNotRisingJumpFlag
    , _wallProximity         = PlayerWallProximityNone
    }

updatePlayerFlags :: PlayerFlags -> PlayerFlags
updatePlayerFlags flags = flags
    { _movingLeftRight       = False
    , _touchingWall          = False
    , _touchingWallNearTop   = False
    , _touchingLeftWall      = False
    , _touchingRightWall     = False
    , _touchingRoof          = False
    , _touchingPlatform      = False
    , _onSpeedRail           = False
    , _onMovingLeftPlatform  = False
    , _onMovingRightPlatform = False
    , _jumped                = False
    , _doubleJumped          = False
    , _walkCanceled          = False
    , _onPlatform            = False
    , _platformDropped       = False
    , _attacked              = False
    , _firedGun              = False
    , _movementSkilled       = False
    , _willFallOffGround     = False
    , _phased                = False
    , _touchingInfoSign      = False
    , _wallProximity         = PlayerWallProximityNone
    }
