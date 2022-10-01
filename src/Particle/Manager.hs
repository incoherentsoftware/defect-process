module Particle.Manager
    ( module Particle.Manager.Types
    , mkParticleManager
    , updateParticleManager
    , drawParticleManager
    ) where

import Control.Monad.State (execState, modify)
import Data.Foldable       (foldlM, for_)

import AppEnv
import Constants
import Msg
import Particle.Manager.Types
import Particle.Types
import Util

mkParticleManager :: ParticleManager
mkParticleManager = ParticleManager
    { _particles = []
    }

addParticleManagerParticles :: ParticleManager -> AppEnv UpdateParticleMsgsPhase ParticleManager
addParticleManagerParticles particleManager = foldlM processMsg particleManager =<< readMsgs
    where
        processMsg :: ParticleManager -> ParticleMsgPayload -> AppEnv UpdateParticleMsgsPhase ParticleManager
        processMsg !pm d = do
            let particles = _particles pm
            particles'   <- case d of
                ParticleMsgAdd p      -> return $ p:particles
                ParticleMsgAddM mkP   -> (:particles) <$> mkP
                ParticleMsgAddsM mkPs -> (++ particles) <$> mkPs

            return $ pm {_particles = particles'}

updateParticleManager :: ParticleManager -> AppEnv UpdateParticleMsgsPhase ParticleManager
updateParticleManager particleManager = addParticleManagerParticles $ particleManager {_particles = particles'}
    where
        particles' = flip execState (_particles particleManager) $ do
            modify $ \ps ->
                [Some $ (_update p) p | Some p <- ps]
            modify $ \ps ->
                [Some $ p {_ttl = _ttl p - timeStep} | Some p <- ps]
            modify $ filter (\(Some p) -> _ttl p > 0.0)

drawParticleManager :: ParticleManager -> AppEnv DrawMsgsPhase ()
drawParticleManager particleManager = for_ (_particles particleManager) $ \(Some p) ->
    (_draw p) p
