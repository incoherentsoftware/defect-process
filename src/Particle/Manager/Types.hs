module Particle.Manager.Types
    ( ParticleManager(..)
    ) where

import Particle
import Util

data ParticleManager = ParticleManager
    { _particles :: [Some Particle]
    }
