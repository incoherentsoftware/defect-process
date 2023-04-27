module Enemy.All.Boss.FlyingProjectile
    ( mkFlyingProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))
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
import InfoMsg.Util
import Msg
import Particle.All.Simple
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

packPath            = \f -> PackResourceFilePath "data/enemies/boss-enemy-attack3.pack" f
hitEffectSpritePath = packPath "attack-flying-projectile-hit.spr" :: PackResourceFilePath

shootFrameTagName = FrameTagName "shoot" :: FrameTagName
shootSpeed        = 650.0                :: Speed
fallbackAimVec    = Vec2 0.0 1.0         :: Vec2

data FlyingProjectileData = FlyingProjectileData
    { _attack :: Attack
    }

mkFlyingProjectile
    :: (ConfigsRead m, MonadIO m)
    => Pos2
    -> AttackDescription
    -> EnemyTauntedStatus
    -> m (Some Projectile)
mkFlyingProjectile pos atkDesc tauntedStatus = do
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
        flyingProjData = FlyingProjectileData {_attack = atk}
        hbx            = fromMaybe (DummyHitbox pos) (attackHitbox atk)

    return . Some $ (mkProjectile flyingProjData msgId hbx maxSecs)
        { _vel                  = zeroVel2
        , _registeredCollisions = S.fromList [ProjRegisteredPlayerCollision]
        , _think                = thinkFlyingProjectile
        , _update               = updateFlyingProjectile
        , _draw                 = drawFlyingProjectile
        , _processCollisions    = processCollisions
        }

thinkFlyingProjectile :: Monad m => ProjectileThink FlyingProjectileData m
thinkFlyingProjectile flyingProj = return . thinkAttack . _attack $ _data flyingProj

isNewlyShootFrame :: Projectile FlyingProjectileData -> Bool
isNewlyShootFrame flyingProj = shootFrameTagName `isAttackFrameTag` atk && attackFrameChanged atk
    where atk = _attack $ _data flyingProj

readPlayerPos :: MsgsRead UpdateProjectileMsgsPhase m => m (Maybe Pos2)
readPlayerPos = processMsgs <$> readMsgs
    where
        processMsgs :: [InfoMsgPayload] -> Maybe Pos2
        processMsgs []     = Nothing
        processMsgs (p:ps) = case p of
            InfoMsgPlayer playerInfo -> Just $ playerInfoCenterPos playerInfo
            _                        -> processMsgs ps

calculateFlyingProjectileVel :: MsgsRead UpdateProjectileMsgsPhase m => Projectile FlyingProjectileData -> m Vel2
calculateFlyingProjectileVel flyingProj
    | isNewlyShootFrame flyingProj = do
        aimVec <- readPlayerPos <&> \case
            Nothing        -> fallbackAimVec
            Just playerPos ->
                let pos = hitboxCenter $ projectileHitbox flyingProj
                in toVec2 $ vecNormalize (playerPos `vecSub` pos)
        return $ toVel2 (aimVec `vecMul` shootSpeed)

    | otherwise = return $ P._vel flyingProj

updateFlyingProjectile :: MsgsReadWrite UpdateProjectileMsgsPhase m => ProjectileUpdate FlyingProjectileData m
updateFlyingProjectile flyingProj = do
    vel <- calculateFlyingProjectileVel flyingProj

    let
        flyingProjData   = _data flyingProj
        atk              = _attack (flyingProjData :: FlyingProjectileData)
        pos              = _pos (atk :: Attack) `vecAdd` (toPos2 $ vel `vecMul` timeStep)
        dir              = _dir (atk :: Attack)
        atk'             = updateAttack pos dir atk
        ttl
            | _done atk' = 0.0
            | otherwise  = _ttl flyingProj

    return $ flyingProj
        { _data   = flyingProjData {_attack = atk'} :: FlyingProjectileData
        , _vel    = vel
        , _hitbox = const $ fromMaybe (DummyHitbox pos) (attackHitbox atk')
        , _ttl    = ttl
        }

processCollisions :: ProjectileProcessCollisions FlyingProjectileData
processCollisions collisions flyingProj = case collisions of
    []            -> []
    (collision:_) -> case collision of
        ProjPlayerCollision player ->
            let atk = _attack (_data flyingProj :: FlyingProjectileData)
            in attackCollisionEntityHitMessages player atk
        _                          -> []

drawFlyingProjectile :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw FlyingProjectileData m
drawFlyingProjectile flyingProj =
    let
        atk = _attack (_data flyingProj :: FlyingProjectileData)
        spr = attackSprite atk
        pos = _pos (atk :: Attack)
        vel = P._vel flyingProj
        dir = _dir (atk :: Attack)
    in do
        pos' <- graphicsLerpPos pos vel
        drawSprite pos' dir enemyAttackProjectileZIndex spr
