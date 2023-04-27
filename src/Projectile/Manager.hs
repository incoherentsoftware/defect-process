module Projectile.Manager
    ( module Projectile.Manager.Types
    , mkProjectileManager
    , clearProjectileManager
    , thinkProjectileManager
    , updateProjectileManager
    , drawProjectileManager
    , projectileManagerProjectileSurfaces
    ) where

import Data.Foldable    (foldlM, for_)
import Data.Maybe       (catMaybes)
import Data.Traversable (for)

import AppEnv
import Constants
import Msg
import Particle.All.Fade
import Projectile
import Projectile.Manager.Types
import Util
import World.Surface.Types

mkProjectileManager :: ProjectileManager
mkProjectileManager = ProjectileManager
    { _projectiles             = []
    , _isPendingVoluntaryClear = False
    }

thinkProjectileManager :: ProjectileManager -> AppEnv ThinkProjectileMsgsPhase ProjectileManager
thinkProjectileManager projectileManager = do
    projectileManager' <- addProjectileManagerNewProjectiles projectileManager

    msgs <- for (_projectiles projectileManager') $ \(Some p) ->
        (_think p) p
    writeMsgs $ concat msgs

    return projectileManager'

clearProjectileManager :: ProjectileManager -> ProjectileManager
clearProjectileManager projectileManager = projectileManager {_projectiles = []}

mkNewThinkProjectiles :: AppEnv ThinkProjectileMsgsPhase [Some Projectile]
mkNewThinkProjectiles = foldlM processMsg [] =<< readMsgs
    where
        processMsg
            :: [Some Projectile]
            -> NewThinkProjectileMsgPayload
            -> AppEnv ThinkProjectileMsgsPhase [Some Projectile]
        processMsg !ps d = case d of
            NewThinkProjectileMsgAdd proj    -> return $ proj:ps
            NewThinkProjectileMsgAdds projs  -> return $ projs ++ ps
            NewThinkProjectileMsgAddM proj   -> (:ps) <$> proj
            NewThinkProjectileMsgAddsM projs -> (++ ps) <$> projs

addProjectileManagerNewProjectiles :: ProjectileManager -> AppEnv ThinkProjectileMsgsPhase ProjectileManager
addProjectileManagerNewProjectiles projectileManager = do
    projectiles <- (++ _projectiles projectileManager) <$> mkNewThinkProjectiles
    return $ projectileManager {_projectiles = projectiles}

mkNewUpdateProjectiles :: AppEnv UpdateProjectileMsgsPhase [Some Projectile]
mkNewUpdateProjectiles = foldlM processMsg [] =<< readMsgs
    where
        processMsg
            :: [Some Projectile]
            -> NewUpdateProjectileMsgPayload
            -> AppEnv UpdateProjectileMsgsPhase [Some Projectile]
        processMsg !ps d = case d of
            NewUpdateProjectileMsgAdd proj    -> return $ proj:ps
            NewUpdateProjectileMsgAdds projs  -> return $ projs ++ ps
            NewUpdateProjectileMsgAddM proj   -> (:ps) <$> proj
            NewUpdateProjectileMsgAddsM projs -> (++ ps) <$> projs

updateProjectileManagerVoluntaryClear :: ProjectileManager -> AppEnv UpdateProjectileMsgsPhase ProjectileManager
updateProjectileManagerVoluntaryClear projectileManager
    | _isPendingVoluntaryClear projectileManager =
        let
            processProj :: MsgsWrite UpdateProjectileMsgsPhase m => Some Projectile -> m (Some Projectile)
            processProj (Some p) = case (_voluntaryClear p) p of
                Nothing                 -> return $ Some p
                Just voluntaryClearData -> do
                    writeMsgs [mkMsg $ ParticleMsgAdd (mkFadeParticle voluntaryClearData)]
                    return . Some $ p {_ttl = 0.0}
        in do
            projectiles <- traverse processProj (_projectiles projectileManager)
            return $ projectileManager
                { _projectiles             = projectiles
                , _isPendingVoluntaryClear = False
                }

    | otherwise =
        let
            processMsgs :: [ProjectileMsgPayload] -> ProjectileManager
            processMsgs []     = projectileManager
            processMsgs (d:ds) = case d of
                ProjectileMsgVoluntaryClear -> projectileManager {_isPendingVoluntaryClear = True}
                _                           -> processMsgs ds

        in processMsgs <$> readMsgs

updateProjectileManager :: ProjectileManager -> AppEnv UpdateProjectileMsgsPhase ProjectileManager
updateProjectileManager projectileManager = do
    projectileManager' <- updateProjectileManagerVoluntaryClear projectileManager

    projectiles  <-
        traverse (\(Some p) -> Some <$> updateProjectileMsgs p) (_projectiles projectileManager') >>=
        traverse (\(Some p) -> Some <$> (_update p) p) >>=
        return . updateProjectileTtls
    projectiles' <- (++ projectiles) <$> mkNewUpdateProjectiles

    return $ projectileManager' {_projectiles = projectiles'}

updateProjectileTtls :: [Some Projectile] -> [Some Projectile]
updateProjectileTtls [] = []
updateProjectileTtls (Some p:ps)
    | ttl' <= 0.0 = ps'
    | otherwise   = Some p':ps'
    where
        ttl' = _ttl p - timeStep
        p'   = p {_ttl = ttl'}
        ps'  = updateProjectileTtls ps

drawProjectileManager :: ProjectileManager -> AppEnv DrawMsgsPhase ()
drawProjectileManager projectileManager = for_ (_projectiles projectileManager) $ \(Some p) ->
    (_draw p) p

projectileManagerProjectileSurfaces :: ProjectileManager -> [Surface]
projectileManagerProjectileSurfaces projectileManager = catMaybes
    [(_surface p) p | Some p <- _projectiles projectileManager]
