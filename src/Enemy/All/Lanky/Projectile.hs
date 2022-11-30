module Enemy.All.Lanky.Projectile
    ( mkLankyProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.Set as S

import Attack
import Attack.Hit
import Collision
import Id
import Msg
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

groundSoundPath         = "event:/SFX Events/Enemy/Lanky/attack-pillar-ground-c" :: FilePath
groundSoundFrameTagName = FrameTagName "groundSoundC"                            :: FrameTagName

data LankyProjectileData = LankyProjectileData
    { _attack              :: Attack
    , _groundSoundHashedId :: HashedId
    }

mkLankyProjectileData :: MonadIO m => Attack -> m LankyProjectileData
mkLankyProjectileData atk = do
    groundSoundHashedId <- hashId <$> newId
    return $ LankyProjectileData
        { _attack              = atk
        , _groundSoundHashedId = groundSoundHashedId
        }

mkLankyProjectile :: MonadIO m => Pos2 -> Direction -> AttackDescription -> m (Some Projectile)
mkLankyProjectile pos dir atkDesc = do
    atk           <- mkAttack pos dir atkDesc
    lankyProjData <- mkLankyProjectileData atk

    msgId  <- newId
    let hbx = fromMaybe (DummyHitbox pos) (attackHitbox atk)
    return . Some $ (mkProjectile lankyProjData msgId hbx maxSecs)
        { _registeredCollisions = S.fromList [ProjRegisteredPlayerCollision]
        , _think                = thinkLankyProjectile
        , _update               = updateLankyProjectile
        , _draw                 = drawLankyProjectile
        , _processCollisions    = processCollisions
        }

thinkLankyProjectile :: Monad m => ProjectileThink LankyProjectileData m
thinkLankyProjectile lankyProj = return $ thinkAttack atk ++ soundMsgs
    where
        lankyProjData = _data lankyProj
        atk           = _attack lankyProjData

        soundMsgs
            | groundSoundFrameTagName `isAttackFrameTag` atk =
                let
                    pos      = _pos (atk :: Attack)
                    hashedId = _groundSoundHashedId lankyProjData
                in [mkMsg $ AudioMsgPlaySoundContinuous groundSoundPath hashedId pos]
            | otherwise                                      = []

updateLankyProjectile :: Monad m => ProjectileUpdate LankyProjectileData m
updateLankyProjectile lankyProj = return $ lankyProj
    { _data   = lankyProjData {_attack = atk'} :: LankyProjectileData
    , _hitbox = const $ fromMaybe (DummyHitbox pos) (attackHitbox atk')
    , _ttl    = ttl
    }
    where
        lankyProjData = _data lankyProj
        atk           = _attack (lankyProjData :: LankyProjectileData)
        pos           = _pos (atk :: Attack)
        dir           = _dir (atk :: Attack)
        atk'          = updateAttack pos dir atk
        ttl           = if _done atk' then 0.0 else _ttl lankyProj

processCollisions :: ProjectileProcessCollisions LankyProjectileData
processCollisions collisions lankyProj = case collisions of
    []            -> []
    (collision:_) -> case collision of
        ProjPlayerCollision player -> playerCollision player lankyProj
        _                          -> []

playerCollision :: CollisionEntity e => e -> Projectile LankyProjectileData -> [Msg ThinkCollisionMsgsPhase]
playerCollision player lankyProj = [mkMsgTo (HurtMsgAttackHit atkHit) playerId]
    where
        playerId = collisionEntityMsgId player
        atk      = _attack (_data lankyProj :: LankyProjectileData)
        atkHit   = mkAttackHit atk

drawLankyProjectile :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw LankyProjectileData m
drawLankyProjectile lankyProj = drawSprite pos dir enemyAttackProjectileZIndex spr
    where
        attack = _attack (_data lankyProj :: LankyProjectileData)
        spr    = attackSprite attack
        pos    = _pos (attack :: Attack)
        dir    = _dir (attack :: Attack)
