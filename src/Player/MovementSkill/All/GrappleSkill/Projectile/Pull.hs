module Player.MovementSkill.All.GrappleSkill.Projectile.Pull
    ( grappleProjPullEnemyCollision
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Attack.Hit
import Collision
import Configs.All.PlayerSkill.Grapple
import Enemy.Types
import Msg
import Player.Gun.FireDrawAngle
import Player.MovementSkill as MS
import Player.MovementSkill.All.GrappleSkill.Data
import Player.MovementSkill.All.GrappleSkill.EnemyPullDummyProjectile
import Player.MovementSkill.All.GrappleSkill.Projectile.Data
import Player.MovementSkill.All.GrappleSkill.Projectile.Towards
import Player.MovementSkill.All.GrappleSkill.Projectile.Util
import Projectile as P
import Util
import Window.Graphics

hitSoundFilePath  = "event:/SFX Events/Player/Skills/grapple-pull-hit" :: FilePath
missSoundFilePath = "event:/SFX Events/Player/Skills/grapple-miss"     :: FilePath

pullEnemyVel :: Pos2 -> Pos2 -> GrappleConfig -> Vel2
pullEnemyVel grappleStartPos intersectPos cfg = toVel2 $ enemyVec `vecMul` _enemyPullGrappleSpeed cfg
    where enemyVec = toVec2 . vecNormalize $ grappleStartPos `vecSub` intersectPos

grappleProjPullUpdate :: MsgsRead UpdateProjectileMsgsPhase m => MsgId -> ProjectileUpdate GrappleProjData m
grappleProjPullUpdate enemyId grappleProj = L.foldl' processMsg grappleProj <$> readMsgs
    where
        processMsg :: Projectile GrappleProjData -> InfoMsgPayload -> Projectile GrappleProjData
        processMsg p d = case d of
            InfoMsgEnemyPos enHbx enId
                | enId == enemyId, [startPos, endPos] <- hitboxVertices (projectileHitbox p) ->
                    let
                        dirVec  = vecNormalize $ endPos `vecSub` startPos
                        dist    = vecDist startPos (hitboxBotCenter enHbx)
                        endPos' = startPos `vecAdd` (dirVec `vecMul` dist)
                    in p {_hitbox = const $ lineHitbox startPos endPos'}

            _ -> p

grappleProjPullStartVisualPos :: Pos2 -> Direction -> Projectile GrappleProjData -> Pos2
grappleProjPullStartVisualPos playerPos@(Pos2 playerX playerY) dir grappleProj = playerPos `vecAdd` offset'
    where
        Pos2 endX endY = hitboxEndVertex $ projectileHitbox grappleProj

        angle
            | approxEq (endY - playerY) 0.0 && approxEq (endX - playerX) 0.0 = 0.0
            | otherwise                                                      = atan2 (endY - playerY) (endX - playerX)

        fireDrawAngle = calculateGunFireDrawAngle dir angle
        cfg           = _config (P._data grappleProj :: GrappleProjData)
        offset        = M.findWithDefault zeroPos2 fireDrawAngle (_projPullStartVisualOffsets cfg)
        offset'       = vecFlip offset dir

grappleProjPullEnemyCollision
    :: Pos2
    -> Pos2
    -> Pos2
    -> Enemy d
    -> Projectile GrappleProjData
    -> [Msg ThinkCollisionMsgsPhase]
grappleProjPullEnemyCollision startPos playerPos@(Pos2 playerX _) intersectPos@(Pos2 intersectX _) enemy grappleProj
    | not enemyPullable = grappleProjTowardsEnemyCollision startPos intersectPos enemy grappleProj
    | enemyTooClose     = grappleProjMsgs ++ [mkMsg $ AudioMsgPlaySound missSoundFilePath intersectPos]
    | otherwise         =
        [ mkMsg $ NewUpdateProjectileMsgAddM mkEnemyPullDummyProj
        , mkMsg $ PlayerMsgSetDirection playerDir
        , setGrapplePullFireDrawStateMsg
        , mkMsgEx (PlayerMsgUpdateMovementSkill updatePull) MsgEndOrder
        , mkMsg $ WorldMsgHitlag (_projHitlag cfg)
        , mkMsgTo (HurtMsgAttackHit grappleHit) enemyId
        , mkMsg $ AudioMsgPlaySound hitSoundFilePath intersectPos
        ] ++ grappleProjMsgs ++ grappleProjHitEffectMsgs intersectPos
    where
        grappleProjData            = P._data grappleProj
        cfg                        = _config (grappleProjData :: GrappleProjData)
        playerPullOffsetX          = _playerPullOffsetX cfg
        offsetX
            | playerX > intersectX = -playerPullOffsetX
            | otherwise            = playerPullOffsetX
        offsetPos                  = Pos2 offsetX 0.0
        grappleStartPos            = grappleProjStartPos grappleProj `vecAdd` offsetPos
        enemyTooClose              = vecDist intersectPos grappleStartPos < _playerPullMinDistance cfg
        enemyId                    = collisionEntityMsgId enemy
        enemyPullable              = (_pullable enemy) enemy
        pullVel                    = pullEnemyVel grappleStartPos intersectPos cfg
        grappleProjId              = P._msgId grappleProj
        mkEnemyPullDummyProj       = mkEnemyPullDummyProjectile grappleStartPos pullVel enemyId cfg
        playerDir
            | playerX > intersectX = LeftDir
            | otherwise            = RightDir

        grappleProjMsgs =
            let
                startVisualPos = grappleProjPullStartVisualPos playerPos playerDir
                updateProj     = \p -> p
                    { _data                 = (P._data p) {_startVisualPos = startVisualPos}
                    , _ttl                  = _projPullSecs cfg
                    , _update               = grappleProjPullUpdate enemyId
                    , _draw                 = drawGrappleProjPull
                    , _registeredCollisions = S.empty
                    }
            in [mkMsgTo (ProjectileMsgUpdate updateProj) grappleProjId]

        grappleHit = (mkAttackHitEmpty grappleProjId intersectPos)
            { _vel            = pullVel
            , _dir            = Just $ if vecX pullVel > 0.0 then LeftDir else RightDir
            , _damage         = _projDamage cfg
            , _stagger        = _projStagger cfg
            , _alwaysLaunches = True
            , _isRanged       = True
            }

        updatePull = \ms -> ms
            { MS._data  = (MS._data ms)
                { _status          = PullGrappling $ _playerPullActiveMaxSecs cfg
                , _projectileMsgId = grappleProjId
                } :: GrappleData
            , MS._status = ActiveCancelableMovement
            }

drawGrappleProjPull :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw GrappleProjData m
drawGrappleProjPull grappleProj = case hitboxVertices (projectileHitbox grappleProj) of
    [startPos, endPos]
        | vecDist startPos endPos <= _playerPullOffsetX cfg -> return ()
    _                                                       -> drawGrappleProj grappleProj
    where cfg = _config (P._data grappleProj :: GrappleProjData)
