module Enemy.All.Boss.WallProjectile
    ( mkWallProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.Set as S

import Attack
import Collision
import Configs
import Constants
import Enemy.TauntedData
import Enemy.Util
import FileCache
import Id
import Msg
import Particle.All.Simple
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

packPath            = \f -> PackResourceFilePath "data/enemies/boss-enemy-attack3.pack" f
hitEffectSpritePath = packPath "attack-wall-projectile-hit.spr" :: PackResourceFilePath

leftDisappearOffsetX = -100.0 :: OffsetX
wallAccelerationX    = -300.0 :: Acceleration

data WallProjectileData = WallProjectileData
    { _attack                  :: Attack
    , _leftDisappearThresholdX :: PosX
    }

mkWallProjectile
    :: (ConfigsRead m, MonadIO m)
    => Pos2
    -> AttackDescription
    -> PosX
    -> EnemyTauntedStatus
    -> m (Some Projectile)
mkWallProjectile pos atkDesc knownInnerLeftWallX tauntedStatus = do
    msgId <- newId
    let
        loadHitParticle = \atk ->
            let atkPos = _pos (atk :: Attack)
            in loadSimpleParticle atkPos LeftDir worldEffectZIndex hitEffectSpritePath
        atkDesc'        = atkDesc
            { _onHitType = AddedOnHitType $ \_ _ atk ->
                [ mkMsg $ ParticleMsgAddM (loadHitParticle atk)
                , mkMsgTo (ProjectileMsgSetTtl 0.0) msgId
                ]
            }
    atk <- mkEnemyAttack pos LeftDir atkDesc' tauntedStatus

    let
        wallProjData = WallProjectileData
            { _attack                  = atk
            , _leftDisappearThresholdX = knownInnerLeftWallX + leftDisappearOffsetX
            }
        hbx          = fromMaybe (DummyHitbox pos) (attackHitbox atk)

    return . Some $ (mkProjectile wallProjData msgId hbx maxSecs)
        { _vel                  = zeroVel2
        , _registeredCollisions = S.fromList [ProjRegisteredPlayerCollision]
        , _think                = thinkWallProjectile
        , _update               = updateWallProjectile
        , _draw                 = drawWallProjectile
        , _processCollisions    = processCollisions
        }

thinkWallProjectile :: Monad m => ProjectileThink WallProjectileData m
thinkWallProjectile wallProj = return . thinkAttack . _attack $ _data wallProj

updateWallProjectile :: MsgsWrite UpdateProjectileMsgsPhase m => ProjectileUpdate WallProjectileData m
updateWallProjectile wallProj = return $ wallProj
    { _data   = wallProjData {_attack = atk'} :: WallProjectileData
    , _vel    = vel
    , _hitbox = const $ fromMaybe (DummyHitbox pos) (attackHitbox atk')
    , _ttl    = ttl
    }
    where
        wallProjData   = _data wallProj
        atk            = _attack (wallProjData :: WallProjectileData)
        Vel2 velX velY = P._vel wallProj
        vel            = Vel2 (velX + wallAccelerationX * timeStep) velY
        pos@(Pos2 x _) = _pos (atk :: Attack) `vecAdd` (toPos2 $ vel `vecMul` timeStep)
        dir            = _dir (atk :: Attack)
        atk'           = updateAttack pos dir atk

        ttl
            | x < _leftDisappearThresholdX wallProjData = 0.0
            | otherwise                                 = _ttl wallProj

processCollisions :: ProjectileProcessCollisions WallProjectileData
processCollisions collisions wallProj = case collisions of
    []            -> []
    (collision:_) -> case collision of
        ProjPlayerCollision player ->
            let atk = _attack (_data wallProj :: WallProjectileData)
            in attackCollisionEntityHitMessages player atk
        _                          -> []

drawWallProjectile :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw WallProjectileData m
drawWallProjectile wallProj =
    let
        atk = _attack (_data wallProj :: WallProjectileData)
        spr = attackSprite atk
        pos = _pos (atk :: Attack)
        vel = P._vel wallProj
        dir = _dir (atk :: Attack)
    in do
        vel' <- toVel2 <$> graphicsLerpPos (toPos2 vel) (Vel2 wallAccelerationX 0.0)
        pos' <- graphicsLerpPos pos vel'
        drawSprite pos' dir enemyAttackProjectileZIndex spr
