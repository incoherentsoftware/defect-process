module Particle.Types
    ( Particle(..)
    , ParticleDraw
    , ParticleUpdate
    ) where

import AppEnv
import Msg.Phase
import Util

type ParticleDraw d m = Particle d -> m ()
type ParticleUpdate d = Particle d -> Particle d

data Particle d = Particle
    { _data   :: d
    , _pos    :: Pos2
    , _vel    :: Vel2
    , _ttl    :: Float
    , _angle  :: Radians
    , _draw   :: ParticleDraw d (AppEnv DrawMsgsPhase)
    , _update :: ParticleUpdate d
    }
