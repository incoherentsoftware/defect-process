module Enemy.All.Boss.LankyProjectile
    ( mkLankyProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.Set as S

import Attack
import Attack.Hit
import Attack.Projectile
import Collision
import Constants
import Enemy.All.Boss.AttackDescriptions
import Enemy.All.Boss.Data
import Enemy.Types as E
import Id
import InfoMsg.Util
import Msg
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

data LankyProjectileData = LankyProjectileData
    { _isReappearAtk     :: Bool
    , _attack            :: Attack
    , _reappearAtkDesc   :: AttackDescription
    , _noReappearAtkDesc :: AttackDescription
    , _enemyMsgId        :: MsgId
    }

mkLankyProjectile :: MonadIO m => Pos2 -> Direction -> MsgId -> BossEnemyData -> Bool -> m (Some Projectile)
mkLankyProjectile enemyPos dir enemyMsgId enemyData isReappearAtk = do
    let
        pos      = enemyPos
        atkDescs = _attackDescs enemyData
    atk <- mkAttack pos dir (_lankyProjectileIndicator atkDescs)

    let
        lankyProjData = LankyProjectileData
            { _isReappearAtk     = isReappearAtk
            , _attack            = atk
            , _reappearAtkDesc   = _lankyReappear atkDescs
            , _noReappearAtkDesc = _lankyProjectile atkDescs
            , _enemyMsgId        = enemyMsgId
            }

    msgId  <- newId
    let hbx = fromMaybe (DummyHitbox pos) (attackHitbox atk)
    return . Some $ (mkProjectile lankyProjData msgId hbx maxSecs)
        { _vel                  = attackVelToVel2 (attackVel atk) zeroVel2
        , _registeredCollisions = S.fromList [ProjRegisteredPlayerCollision]
        , _think                = thinkLankyProjectile
        , _update               = updateLankyProjectile
        , _draw                 = drawLankyProjectile
        , _processCollisions    = processCollisions
        }

thinkLankyProjectile :: Monad m => ProjectileThink LankyProjectileData m
thinkLankyProjectile lankyProj = return $ atkMsgs ++ reappearMsgs
    where
        lankyProjData  = P._data lankyProj
        atk            = _attack (lankyProjData :: LankyProjectileData)
        atkMsgs        = thinkAttack atk
        pos@(Pos2 x _) = _pos (atk :: Attack)

        reappearMsgs
            | attackIsLastFrameIndex atk && attackFrameChanged atk = if
                | _isReappearAtk lankyProjData ->
                    let
                        setReappearAtk = \e ->
                            let
                                dir = case _knownPlayerInfo e of
                                    Nothing                                   -> _dir (atk :: Attack)
                                    Just playerInfo
                                        | vecX (playerInfoPos playerInfo) < x -> LeftDir
                                        | otherwise                           -> RightDir
                            in do
                                reappearAtk <- mkAttack pos dir (_reappearAtkDesc lankyProjData)
                                return $ e
                                    { E._pos    = pos
                                    , E._dir    = dir
                                    , E._attack = Just reappearAtk
                                    }
                    in [mkMsgTo (EnemyMsgUpdateM @BossEnemyData setReappearAtk) (_enemyMsgId lankyProjData)]

                | otherwise ->
                    let
                        dir               = _dir (atk :: Attack)
                        noReappearAtkDesc = _noReappearAtkDesc lankyProjData
                    in [mkMsg $ NewUpdateProjectileMsgAddM (mkEnemyAttackProjectile pos dir noReappearAtkDesc)]

            | otherwise = []

updateLankyProjectile :: Monad m => ProjectileUpdate LankyProjectileData m
updateLankyProjectile lankyProj = return $ lankyProj
    { _data   = lankyProjData'
    , _vel    = vel
    , _hitbox = const $ fromMaybe (DummyHitbox pos') (attackHitbox atk')
    , _ttl    = ttl
    }
    where
        lankyProjData = P._data lankyProj
        atk           = _attack (lankyProjData :: LankyProjectileData)
        pos           = _pos (atk :: Attack)
        vel           = attackVelToVel2 (attackVel atk) zeroVel2
        pos'          = pos `vecAdd` (toPos2 $ vel `vecMul` timeStep)
        dir           = _dir (atk :: Attack)

        atk'           = updateAttack pos' dir atk
        ttl            = if _done atk' then 0.0 else _ttl lankyProj
        lankyProjData' = lankyProjData {_attack = atk'} :: LankyProjectileData

processCollisions :: ProjectileProcessCollisions LankyProjectileData
processCollisions collisions lankyProj = foldr processCollision [] collisions
    where
        processCollision :: ProjectileCollision -> [Msg ThinkCollisionMsgsPhase] -> [Msg ThinkCollisionMsgsPhase]
        processCollision pc msgs = case pc of
            ProjPlayerCollision player -> playerCollision player lankyProj ++ msgs
            _                          -> msgs

playerCollision :: CollisionEntity e => e -> Projectile LankyProjectileData -> [Msg ThinkCollisionMsgsPhase]
playerCollision player lankyProj =
    [ mkMsgTo (HurtMsgAttackHit atkHit) playerId
    , mkMsgTo (ProjectileMsgSetTtl 0.0) lankyProjId
    ]
    where
        playerId    = collisionEntityMsgId player
        lankyProjId = P._msgId lankyProj
        atk         = _attack (P._data lankyProj :: LankyProjectileData)
        atkHit      = mkAttackHit atk

drawLankyProjectile :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw LankyProjectileData m
drawLankyProjectile lankyProj =
    let
        attack = _attack (P._data lankyProj :: LankyProjectileData)
        spr    = attackSprite attack
        pos    = _pos (attack :: Attack)
        vel    = P._vel lankyProj
        dir    = _dir (attack :: Attack)
    in do
        pos' <- graphicsLerpPos pos vel
        drawSprite pos' dir enemyAttackProjectileZIndex spr
