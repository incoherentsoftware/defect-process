module Player.MovementSkill.All.GrappleSkill.Projectile.Towards
    ( grappleProjTowardsNonEnemyCollision
    , grappleProjTowardsEnemyCollision
    ) where

import qualified Data.List as L
import qualified Data.Set  as S

import Attack.Hit
import Collision
import Configs.All.PlayerSkill.Grapple
import Constants
import InfoMsg.Util
import Msg
import Player.MovementSkill as MS
import Player.MovementSkill.All.GrappleSkill.Data
import Player.MovementSkill.All.GrappleSkill.Projectile.Data
import Player.MovementSkill.All.GrappleSkill.Projectile.Util
import Projectile as P
import Util

hitSoundFilePath = "event:/SFX Events/Player/Skills/grapple-hit" :: FilePath

initialVelYUnscaled = -600.0 :: Float

grappleProjTowardsUpdate :: MsgsRead UpdateProjectileMsgsPhase m => ProjectileUpdate GrappleProjData m
grappleProjTowardsUpdate grappleProj =
    let
        processInfoMsg :: Projectile GrappleProjData -> InfoMsgPayload -> Projectile GrappleProjData
        processInfoMsg proj d = case d of
            InfoMsgPlayer playerInfo
                | [startPos, endPos] <- hitboxVertices (projectileHitbox proj) ->
                    let
                        playerPos = playerInfoPos playerInfo
                        dirVec    = vecNormalize $ startPos `vecSub` endPos
                        dist      = vecDist endPos playerPos
                        startPos' = endPos `vecAdd` (dirVec `vecMul` dist)
                    in proj
                        { _data   = (P._data proj) {_playerPos = playerPos}
                        , _hitbox = const $ lineHitbox startPos' endPos
                        }

            _ -> proj

        processMovingPlatformMsgs :: Projectile GrappleProjData -> [CollisionMsgPayload] -> Projectile GrappleProjData
        processMovingPlatformMsgs proj []     = proj
        processMovingPlatformMsgs proj (p:ps) = case p of
            CollisionMsgMovingPlatform platHbx projectedPlatHbx
                | projHbx `intersectsHitbox` projectedPlatHbx ->
                    let
                        offset   = Pos2 (hitboxLeft projectedPlatHbx - hitboxLeft platHbx) 0.0
                        startPos = hitboxStartVertex projHbx
                        endPos   = hitboxEndVertex projHbx
                        -- accumulates floating point error, but doesn't matter since short-lived
                        endPos'  = endPos `vecAdd` offset
                    in proj {_hitbox = const $ lineHitbox startPos endPos'}

            _ -> processMovingPlatformMsgs proj ps

            where projHbx = projectileHitbox proj
    in do
        grappleProj' <- L.foldl' processInfoMsg grappleProj <$> readMsgs
        processMovingPlatformMsgs grappleProj' <$> readMsgs

grappleProjTowardsCollision
    :: Pos2
    -> Overshoot
    -> Maybe Direction
    -> Projectile GrappleProjData
    -> [Msg ThinkCollisionMsgsPhase]
grappleProjTowardsCollision intersectPos@(Pos2 intersectX _) overshoot faceDir grappleProj =
    [ mkMsgTo (ProjectileMsgUpdate updateProj) grappleProjId
    , mkMsgEx (PlayerMsgSetVelocity initialVel) MsgEndOrder
    , mkMsgEx (PlayerMsgSetDirection dir) MsgAfterNormalOrder
    , mkMsgEx (PlayerMsgUpdateMovementSkill updateTowards) MsgEndOrder
    , mkMsg $ AudioMsgPlaySound hitSoundFilePath intersectPos
    , mkMsg $ WorldMsgHitlag (_projHitlag cfg)
    ]
        where
            grappleProjData      = P._data grappleProj
            cfg                  = _config (grappleProjData :: GrappleProjData)
            targetVel            = playerTowardsVel (grappleProjStartPos grappleProj) intersectPos cfg
            Pos2 grappleStartX _ = grappleProjStartPos grappleProj
            dir                  = if intersectX < grappleStartX then LeftDir else RightDir
            grappleProjId        = P._msgId grappleProj

            initialVelY
                | _playerTouchingGround grappleProjData && vecY targetVel > 0.0 =
                    let targetVecY = vecY $ vecNormalize (toVec2 targetVel)
                    in initialVelYUnscaled * targetVecY
                | otherwise                                                     = 0.0
            initialVel
                | initialVelY < 0.0 = Vel2 (vecX targetVel) initialVelY
                | otherwise         = targetVel

            projStartVisualPos = \p ->
                let
                    pData       = P._data p
                    playerPos   = _playerPos pData
                    playerPos'  = playerPos `vecAdd` (toPos2 $ targetVel `vecMul` timeStep)
                    handsOffset = _playerTowardsHandsOffset cfg `vecFlip` dir
                in playerPos' `vecAdd` handsOffset

            updateProj = \p -> p
                { _data                 = (P._data p)
                    { _startVisualPos = projStartVisualPos
                    , _startPosVel    = targetVel
                    }
                , _ttl                  = _projTowardsSecs cfg
                , _update               = grappleProjTowardsUpdate
                , _registeredCollisions = S.empty
                }

            updateTowards = \ms ->
                let
                    grappleStatus =
                        TowardsGrappling intersectPos initialVelY (_playerTowardsActiveMaxSecs cfg) overshoot faceDir
                in ms
                    { MS._data = (MS._data ms)
                        { _status          = grappleStatus
                        , _projectileMsgId = P._msgId grappleProj
                        } :: GrappleData
                    , _status  = ActiveCancelableMovement
                    }

grappleProjTowardsNonEnemyCollision :: Pos2 -> Hitbox -> Projectile GrappleProjData -> [Msg ThinkCollisionMsgsPhase]
grappleProjTowardsNonEnemyCollision intersectPos collisionHbx grappleProj =
    effectMsg:grappleProjTowardsCollision intersectPos AllowOvershoot Nothing grappleProj
        where effectMsg = grappleProjSurfaceEffectMsg intersectPos collisionHbx

grappleProjTowardsEnemyCollision
    :: CollisionEntity e
    => Pos2
    -> Pos2
    -> e
    -> Projectile GrappleProjData
    -> [Msg ThinkCollisionMsgsPhase]
grappleProjTowardsEnemyCollision startPos intersectPos enemy grappleProj =
    [ mkMsgToEx (EnemyMsgSetHangtime $ _enemyTowardsHangtimeSecs cfg) enemyId MsgAfterNormalOrder
    , mkMsgTo (HurtMsgAttackHit grappleHit) enemyId
    ] ++ towardsMsgs ++ grappleProjHitEffectMsgs intersectPos
        where
            cfg                        = _config (P._data grappleProj :: GrappleProjData)
            startX                     = vecX startPos
            Pos2 intersectX intersectY = intersectPos
            enemyHbx                   = collisionEntityHitbox enemy
            hbxLeft                    = hitboxLeft enemyHbx
            hbxRight                   = hitboxRight enemyHbx
            xOffset                    = _playerTowardsXOffset cfg
            (intersectX', faceDir)
                | intersectX < startX  = (hbxRight + xOffset, Just LeftDir)
                | otherwise            = (hbxLeft - xOffset, Just RightDir)
            intersectPos'              = Pos2 intersectX' intersectY
            hitstunMultiplier          =
                (vecDist startPos intersectPos / _playerGrappleSpeed cfg) * _projHitstunMultiplier cfg

            grappleProjId = P._msgId grappleProj
            grappleHit    = (mkAttackHitEmpty grappleProjId intersectPos')
                { _dir               = Just $ if vecX intersectPos' > vecX startPos then RightDir else LeftDir
                , _damage            = _projDamage cfg
                , _stagger           = _projStagger cfg
                , _isRanged          = True
                , _hitstunMultiplier = hitstunMultiplier
                }

            enemyId     = collisionEntityMsgId enemy
            towardsMsgs = grappleProjTowardsCollision intersectPos' PreventOvershoot faceDir grappleProj
