module Attack.Hit.Types
    ( AttackHitLaunchTarget(..)
    , AttackHit(..)
    ) where

import Attack.Description.Types
import Attack.Util
import Collision
import Id
import Particle.All.AttackSpecks.Types
import Util

data AttackHitLaunchTarget = AttackHitLaunchTarget
    { _posY           :: PosY
    , _offsetY        :: PosY
    , _allowOvershoot :: Bool
    }

data AttackHit = AttackHit
    { _hashedId          :: HashedId
    , _intersectPos      :: Pos2
    , _hitbox            :: Maybe Hitbox
    , _vel               :: Vel2
    , _dir               :: Maybe Direction
    , _launchTarget      :: Maybe AttackHitLaunchTarget
    , _damage            :: Damage
    , _stagger           :: Stagger
    , _alwaysLaunches    :: Bool
    , _isWeakVel         :: Bool
    , _isRanged          :: Bool
    , _hitstunMultiplier :: Float
    , _hitEffectType     :: AttackHitEffectType
    , _specksType        :: Maybe AttackSpecksType
    , _specksPos         :: Maybe AttackSpecksPosition
    , _specksDirection   :: Maybe AttackSpecksDirection
    }
