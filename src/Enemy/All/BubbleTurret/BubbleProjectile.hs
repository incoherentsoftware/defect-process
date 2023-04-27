module Enemy.All.BubbleTurret.BubbleProjectile
    ( bubbleSpinPath
    , bubbleExplodePath
    , mkBubbleProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Set as S

import Attack
import Collision
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.BubbleTurret
import Constants
import Enemy.TauntedData
import Enemy.Util
import FileCache
import Id
import Msg
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

bubbleSpinPath    =
    PackResourceFilePath "data/enemies/bubble-turret-enemy.pack" "bubble-spin.spr"    :: PackResourceFilePath
bubbleExplodePath =
    PackResourceFilePath "data/enemies/bubble-turret-enemy.pack" "bubble-explode.atk" :: PackResourceFilePath

registeredCollisions = S.fromList
    [ ProjRegisteredPlayerCollision
    ] :: S.Set ProjectileRegisteredCollision

data BubbleProjVelBehavior
    = InitialRiseVel Secs
    | RiseFallVel Secs

data BubbleProjData = BubbleProjData
    { _velBehavior    :: BubbleProjVelBehavior
    , _pos            :: Pos2
    , _dir            :: Direction
    , _sprite         :: Sprite
    , _explodeAtkDesc :: AttackDescription
    , _tauntedStatus  :: EnemyTauntedStatus
    , _config         :: BubbleTurretEnemyConfig
    }

mkBubbleProjData
    :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m)
    => Pos2
    -> Direction
    -> EnemyTauntedStatus
    -> m BubbleProjData
mkBubbleProjData pos dir tauntedStatus = do
    spr            <- loadPackSprite bubbleSpinPath
    explodeAtkDesc <- loadPackAttackDescription bubbleExplodePath
    cfg            <- readConfig _enemy _bubbleTurret

    return $ BubbleProjData
        { _velBehavior    = InitialRiseVel $ _bubbleProjInitialRiseSecs cfg
        , _pos            = pos
        , _dir            = dir
        , _sprite         = spr
        , _explodeAtkDesc = explodeAtkDesc
        , _tauntedStatus  = tauntedStatus
        , _config         = cfg
        }

bubbleProjHitbox :: ProjectileHitbox BubbleProjData
bubbleProjHitbox bubbleProj = rectHitbox pos width height
    where
        bubbleProjData = _data bubbleProj
        Pos2 x y       = _pos (bubbleProjData :: BubbleProjData)
        cfg            = _config (bubbleProjData :: BubbleProjData)
        width          = _bubbleProjWidth cfg
        height         = _bubbleProjHeight cfg
        pos            = Pos2 (x - width / 2.0) (y - height / 2.0)

mkBubbleProjectile
    :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m)
    => Pos2
    -> Direction
    -> EnemyTauntedStatus
    -> m (Some Projectile)
mkBubbleProjectile pos dir tauntedStatus = do
    bubbleProjData <- mkBubbleProjData pos dir tauntedStatus
    msgId          <- newId

    let
        dummyHbx = dummyHitbox pos
        ttl      = _bubbleProjAliveSecs $ _config (bubbleProjData :: BubbleProjData)

    return . Some $ (mkProjectile bubbleProjData msgId dummyHbx ttl)
        { _hitbox               = bubbleProjHitbox
        , _registeredCollisions = registeredCollisions
        , _think                = thinkBubbleProj
        , _update               = updateBubbleProj
        , _draw                 = drawBubbleProj
        , _processCollisions    = processBubbleProjCollisions
        , _voluntaryClear       = voluntaryClearData
        }

bubbleProjExplodeRemoveMsgs
    :: (AllowMsgWrite p NewUpdateProjectileMsgPayload, AllowMsgWrite p ProjectileMsgPayload)
    => Projectile BubbleProjData
    -> [Msg p]
bubbleProjExplodeRemoveMsgs bubbleProj = [mkAtkProjMsg, removeBubbleProjMsg]
    where
        bubbleProjData = _data bubbleProj
        pos            = _pos (bubbleProjData :: BubbleProjData)
        dir            = _dir (bubbleProjData :: BubbleProjData)
        explodeAtkDesc = _explodeAtkDesc bubbleProjData
        mkAtkProj      = mkEnemyAttackProjectile pos dir explodeAtkDesc EnemyTauntedInactive
        mkAtkProjMsg   = mkMsg $ NewUpdateProjectileMsgAddM mkAtkProj

        bubbleProjId        = P._msgId bubbleProj
        removeBubbleProjMsg = mkMsgTo (ProjectileMsgSetTtl 0.0) bubbleProjId

thinkBubbleProj :: Monad m => ProjectileThink BubbleProjData m
thinkBubbleProj bubbleProj = return $ if
    | willDisappear -> bubbleProjExplodeRemoveMsgs bubbleProj

    | otherwise ->
        let
            bubbleProjData       = _data bubbleProj
            cfg                  = _config (bubbleProjData :: BubbleProjData)
            speedX               = _bubbleProjSpeedX cfg
            speedY               = _bubbleProjSpeedY cfg
            riseFallPeriodSecs   = _bubbleProjRiseFallPeriodSecs cfg
            dir                  = _dir (bubbleProjData :: BubbleProjData)
            velX                 = speedX * directionNeg dir
            velY                 = vecY $ P._vel bubbleProj
            (velBehavior, velY') = case _velBehavior bubbleProjData of
                InitialRiseVel velTtl
                    | velTtl <= 0.0 -> (RiseFallVel riseFallPeriodSecs, speedY)
                    | otherwise     -> (InitialRiseVel (velTtl - timeStep), -speedY)
                RiseFallVel velTtl
                    | velTtl <= 0.0 -> (RiseFallVel riseFallPeriodSecs, -velY)
                    | otherwise     -> (RiseFallVel (velTtl - timeStep), velY)

            update = \p -> p
                { _data = (P._data p) {_velBehavior = velBehavior}
                , _vel  = Vel2 velX velY'
                }
        in [mkMsgTo (ProjectileMsgUpdate update) (P._msgId bubbleProj)]

    where willDisappear = P._ttl bubbleProj - timeStep <= 0.0

updateBubbleProj :: Monad m => ProjectileUpdate BubbleProjData m
updateBubbleProj bubbleProj = return $ bubbleProj {_data = bubbleProjData'}
    where
        bubbleProjData  = _data bubbleProj
        pos             = _pos (bubbleProjData :: BubbleProjData)
        vel             = P._vel bubbleProj
        pos'            = pos `vecAdd` (toPos2 $ vel `vecMul` timeStep)
        spr             = _sprite (bubbleProjData :: BubbleProjData)
        bubbleProjData' = bubbleProjData
            { _pos    = pos'
            , _sprite = updateSprite spr
            } :: BubbleProjData

drawBubbleProj :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw BubbleProjData m
drawBubbleProj bubbleProj = drawSprite pos dir enemyAttackProjectileZIndex spr
    where
        bubbleProjData = _data bubbleProj
        pos            = _pos (bubbleProjData :: BubbleProjData)
        dir            = _dir (bubbleProjData :: BubbleProjData)
        spr            = _sprite (bubbleProjData :: BubbleProjData)

processBubbleProjCollisions :: ProjectileProcessCollisions BubbleProjData
processBubbleProjCollisions collisions bubbleProj = foldr processCollision [] collisions
    where
        processCollision :: ProjectileCollision -> [Msg ThinkCollisionMsgsPhase] -> [Msg ThinkCollisionMsgsPhase]
        processCollision collision !msgs = case collision of
            ProjPlayerCollision _ -> bubbleProjExplodeRemoveMsgs bubbleProj ++ msgs
            _                     -> msgs

voluntaryClearData :: ProjectileVoluntaryClear BubbleProjData
voluntaryClearData bubbleProj = case spriteImage spr of
    Nothing  -> Nothing
    Just img -> Just $ ProjectileVoluntaryClearData
        { _pos    = _pos (bubbleProjData :: BubbleProjData)
        , _dir    = _dir (bubbleProjData :: BubbleProjData)
        , _zIndex = enemyAttackProjectileZIndex
        , _image  = img
        }
    where
        bubbleProjData = _data bubbleProj
        spr            = _sprite (bubbleProjData :: BubbleProjData)
