module Player.Momentum
    ( MomentumOverride(..)
    , PlayerMomentum(..)
    , mkPlayerMomentum
    , resetPlayerMomentumAir
    , setPlayerMomentumAir
    , setPlayerMomentumGround
    , updatePlayerMomentumAirTtl
    , updatePlayerMomentumGroundFriction
    ) where

import Constants
import Util

newAirVelTimeout    = 0.25 :: Secs
groundFrictionDecel = 7000 :: Float

data MomentumOverride
    = MomentumOverrideAll
    | MomentumOverrideAllTemp
    | MomentumOverrideVert
    | MomentumOverrideNone

data PlayerMomentum = PlayerMomentum
    { _airVel     :: Vel2
    , _airTtl     :: Secs
    , _groundVelX :: VelX
    , _isSliding  :: Bool
    }
    deriving Show

mkPlayerMomentum :: PlayerMomentum
mkPlayerMomentum = PlayerMomentum
    { _airVel     = zeroVel2
    , _airTtl     = 0.0
    , _groundVelX = 0.0
    , _isSliding  = False
    }

resetPlayerMomentumAir :: PlayerMomentum -> PlayerMomentum
resetPlayerMomentumAir momentum = momentum
    { _airVel = zeroVel2
    , _airTtl = 0.0
    }

setPlayerMomentumAir :: Vel2 -> PlayerMomentum -> PlayerMomentum
setPlayerMomentumAir vel momentum = momentum
    { _airVel     = vel
    , _airTtl     = newAirVelTimeout
    , _groundVelX = 0.0
    , _isSliding  = False
    }

setPlayerMomentumGround :: VelX -> PlayerMomentum -> PlayerMomentum
setPlayerMomentumGround velX momentum = momentum
    { _airVel     = zeroVel2
    , _airTtl     = 0.0
    , _groundVelX = velX
    , _isSliding  = False
    }

updatePlayerMomentumAirTtl :: PlayerMomentum -> PlayerMomentum
updatePlayerMomentumAirTtl momentum = momentum
    { _airTtl = max 0.0 (_airTtl momentum - timeStep)
    }

updatePlayerMomentumGroundFriction :: PlayerMomentum -> PlayerMomentum
updatePlayerMomentumGroundFriction momentum = momentum
    { _groundVelX = groundVelX'
    , _isSliding  = not $ groundVelX' `approxEq` 0.0
    }
    where
        frictionDecel                   = groundFrictionDecel * timeStep
        groundVelX                      = _groundVelX momentum
        groundVelX'
            | groundVelX `approxEq` 0.0 = 0.0
            | groundVelX < 0.0          = min 0.0 (groundVelX + frictionDecel)
            | otherwise                 = max 0.0 (groundVelX - frictionDecel)
