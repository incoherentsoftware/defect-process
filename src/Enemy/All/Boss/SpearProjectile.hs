module Enemy.All.Boss.SpearProjectile
    ( mkSpearProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.Set as S

import Attack
import Collision
import Constants
import FileCache
import Id
import Msg
import Particle.All.Simple
import Projectile as P
import Util
import Window.Graphics
import World.Surface
import World.ZIndex

packPath            = \f -> PackResourceFilePath "data/enemies/boss-enemy-attack3.pack" f
hitEffectSpritePath = packPath "attack-spear-projectile-hit.spr" :: PackResourceFilePath

projSurfaceWidth          = 219.0                :: Float
projSurfaceHeight         = 9.0                  :: Float
projSurfaceRightDirOffset = Pos2 (-362.0) (-3.0) :: Pos2
projSurfaceLeftDirOffset  = Pos2 144.0 (-3.0)    :: Pos2

data SpearProjectileData = SpearProjectileData
    { _attack :: Attack
    }

mkSpearProjectile :: MonadIO m => Pos2 -> AttackDescription -> m (Some Projectile)
mkSpearProjectile pos atkDesc = do
    msgId <- newId
    let
        loadHitParticle = \atk -> loadSimpleParticle (_pos atk) LeftDir worldEffectZIndex hitEffectSpritePath
        atkDesc'        = atkDesc
            { _onHitType = AddedOnHitType $ \_ _ atk ->
                [ mkMsg $ ParticleMsgAddM (loadHitParticle atk)
                , mkMsgTo (ProjectileMsgSetTtl 0.0) msgId
                ]
            }
    atk <- mkAttack pos LeftDir atkDesc'

    let
        spearProjData = SpearProjectileData {_attack = atk}
        hbx           = fromMaybe (DummyHitbox pos) (attackHitbox atk)
        vel           = attackVelToVel2 (attackVel atk) zeroVel2

    return . Some $ (mkProjectile spearProjData msgId hbx maxSecs)
        { _vel                  = vel
        , _registeredCollisions = S.fromList [ProjRegisteredPlayerCollision]
        , _surface              = spearProjectileSurface
        , _think                = thinkSpearProjectile
        , _update               = updateSpearProjectile
        , _draw                 = drawSpearProjectile
        , _processCollisions    = processCollisions
        }

spearProjectileSurfaceHitbox :: Projectile SpearProjectileData -> Hitbox
spearProjectileSurfaceHitbox spearProj = rectHitbox pos projSurfaceWidth projSurfaceHeight
    where
        spearProjData = _data spearProj
        atk           = _attack spearProjData
        offset        = case _dir (atk :: Attack) of
            RightDir -> projSurfaceRightDirOffset
            LeftDir  -> projSurfaceLeftDirOffset
        pos           = _pos (atk :: Attack) `vecAdd` offset

spearProjectileSurface :: ProjectileSurface SpearProjectileData
spearProjectileSurface spearProj = Just $ mkPlatformSurface (spearProjectileSurfaceHitbox spearProj)

thinkSpearProjectile :: Monad m => ProjectileThink SpearProjectileData m
thinkSpearProjectile spearProj = return $ movingPlatformMsg:thinkAttack atk
    where
        surfaceHbx          = spearProjectileSurfaceHitbox spearProj
        spearProjData       = _data spearProj
        atk                 = _attack spearProjData
        vel                 = attackVelToVel2 (attackVel atk) zeroVel2
        projectedOffset     = toPos2 $ vel `vecMul` timeStep
        projectedSurfaceHbx = moveHitbox projectedOffset surfaceHbx
        movingPlatformMsg   = mkMsg $ CollisionMsgMovingPlatform surfaceHbx projectedSurfaceHbx

updateSpearProjectile :: MsgsReadWrite UpdateProjectileMsgsPhase m => ProjectileUpdate SpearProjectileData m
updateSpearProjectile spearProj =
    let
        spearProjData    = _data spearProj
        atk              = _attack (spearProjData :: SpearProjectileData)
        pos              = _pos (atk :: Attack) `vecAdd` (toPos2 $ vel `vecMul` timeStep)
        vel              = attackVelToVel2 (attackVel atk) zeroVel2
        dir              = _dir (atk :: Attack)
        atk'             = updateAttack pos dir atk
        ttl
            | _done atk' = 0.0
            | otherwise  = _ttl spearProj
    in return $ spearProj
        { _data   = spearProjData {_attack = atk'} :: SpearProjectileData
        , _vel    = vel
        , _hitbox = const $ fromMaybe (DummyHitbox pos) (attackHitbox atk')
        , _ttl    = ttl
        }

processCollisions :: ProjectileProcessCollisions SpearProjectileData
processCollisions collisions spearProj = case collisions of
    []            -> []
    (collision:_) -> case collision of
        ProjPlayerCollision player ->
            let atk = _attack (_data spearProj :: SpearProjectileData)
            in attackCollisionEntityHitMessages player atk
        _                          -> []

drawSpearProjectile :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw SpearProjectileData m
drawSpearProjectile spearProj =
    let
        atk = _attack (_data spearProj :: SpearProjectileData)
        spr = attackSprite atk
        pos = _pos (atk :: Attack)
        vel = P._vel spearProj
        dir = _dir (atk :: Attack)
    in do
        pos' <- graphicsLerpPos pos vel
        drawSprite pos' dir enemyAttackProjectileZIndex spr
