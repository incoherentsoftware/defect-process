module Particle
    ( module Particle.Types
    , mkParticle
    , particleClosestDirAngle
    ) where

import Data.Maybe (listToMaybe)
import qualified Data.List as L

import Collision.Hitbox
import Particle.Types
import Util

mkParticle :: d -> Pos2 -> Secs -> Particle d
mkParticle dat pos ttl = Particle
    { _data   = dat
    , _pos    = pos
    , _vel    = zeroVel2
    , _ttl    = ttl
    , _angle  = 0.0
    , _draw   = const $ return ()
    , _update = id
    }

particleClosestDirAngle :: Pos2 -> Hitbox -> (Direction, Radians)
particleClosestDirAngle (Pos2 intersectX intersectY) hbx =
    maybe (RightDir, 0.0) snd (listToMaybe $ L.sortOn (abs . fst) distDirAngles)
        where
            distDirAngles =
                [ (intersectX - hitboxLeft hbx, (LeftDir, 0.0))
                , (intersectX - hitboxRight hbx, (RightDir, 0.0))
                , (intersectY - hitboxTop hbx, (LeftDir, pi / 2.0))
                , (intersectY - hitboxBot hbx, (RightDir, pi / 2.0))
                ]
