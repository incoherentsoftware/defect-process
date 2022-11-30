module Player.MovementSkill.All.GrappleSkill.Projectile
    ( mkGrappleProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map as M
import qualified Data.Set as S

import Collision
import Configs.All.PlayerSkill.Grapple
import Constants
import Id
import Level.Room.Item.Types as RI
import Msg
import Player
import Player.AimBody
import Player.Gun.FireDrawAngle
import Player.MovementSkill as MS
import Player.MovementSkill.All.GrappleSkill.Data
import Player.MovementSkill.All.GrappleSkill.Projectile.Data
import Player.MovementSkill.All.GrappleSkill.Projectile.Pull
import Player.MovementSkill.All.GrappleSkill.Projectile.Towards
import Player.MovementSkill.All.GrappleSkill.Projectile.Util
import Projectile as P
import Util

mkGrappleProjectile :: MonadIO m => ShotType -> Player -> MovementSkill GrappleData -> m (Some Projectile)
mkGrappleProjectile shotType player grappleSkill =
    let
        grappleData = MS._data grappleSkill
        cfg         = _config (grappleData :: GrappleData)
        aimAngle    = _aimAngle (grappleData :: GrappleData)
        targetPos   = playerRawAimTargetWithAngle player (_projRange cfg) aimAngle

        playerDir               = _dir player
        fireDrawAngle           = calculateGunFireDrawAngle playerDir aimAngle
        startLeadShoulderOffset =
            maybe zeroPos2 (\v -> vecFlip v playerDir) (fireDrawAngle `M.lookup` _projStartLeadShoulderOffsets cfg)
        aimDir                  = calculateAimAngleDir aimAngle

        startAngle  = calculateAimOverlayAngle aimAngle
        startOffset = vecFlipRotate startLeadShoulderOffset aimDir startAngle
        startPos    = playerShoulderPos player `vecAdd` startOffset

        endPos        = extendGrappleProjEndPos (startPos, startPos) targetPos cfg
        playerId      = _msgId (player :: Player)
        hbx           = lineHitbox startPos endPos
        projData      = mkGrappleProjData shotType player (MS._data grappleSkill)
        projThrowSecs = _projThrowSecs cfg
        playerPos     = _pos (player :: Player)
    in do
        msgId <- newId
        return . Some $ (mkProjectile projData msgId hbx projThrowSecs)
            { _ownerId              = playerId
            , _registeredCollisions = S.fromList
                [ ProjRegisteredSurfaceCollision
                , ProjRegisteredEnemyCollision
                , ProjRegisteredRoomItemCollision
                ]
            , _update               = updateGrappleProjExtend targetPos
            , _draw                 = drawGrappleProj
            , _processCollisions    = processGrappleProjCollisions startPos playerPos
            }

processGrappleProjCollisions :: Pos2 -> Pos2 -> ProjectileProcessCollisions GrappleProjData
processGrappleProjCollisions startPos playerPos projCollisions grappleProj =
    processCollisions $ sortProjectileCollisions projCollisions grappleProj
        where
            processCollisions :: [(Pos2, ProjectileCollision)] -> [Msg ThinkCollisionMsgsPhase]
            processCollisions []                                     = []
            processCollisions ((intersectPos, collision):collisions) = case collision of
                -- grapple stops at first thing hit
                ProjEnemyCollision enemy -> case _shotType (P._data grappleProj) of
                    TowardsShot -> grappleProjTowardsEnemyCollision startPos intersectPos enemy grappleProj
                    PullShot    -> grappleProjPullEnemyCollision startPos playerPos intersectPos enemy grappleProj

                ProjSurfaceCollision surfaceHbx _ ->
                    grappleProjTowardsNonEnemyCollision intersectPos surfaceHbx grappleProj

                ProjRoomItemCollision (Some item) ->
                    grappleProjTowardsNonEnemyCollision intersectPos (RI._hitbox item) grappleProj

                _ -> processCollisions collisions

extendGrappleProjEndPos :: (Pos2, Pos2) -> Pos2 -> GrappleConfig -> Pos2
extendGrappleProjEndPos (startPos, endPos) targetPos cfg
    | vecDistSq endPos' startPos >= vecDistSq targetPos startPos = targetPos
    | otherwise                                                  = endPos'
    where
        targetDir = toVec2 . vecNormalize $ targetPos `vecSub` startPos
        endOffset = toPos2 $ targetDir `vecMul` (_projSpeed cfg * timeStep)
        endPos'   = endPos `vecAdd` endOffset

updateGrappleProjExtend :: Monad m => Pos2 -> ProjectileUpdate GrappleProjData m
updateGrappleProjExtend targetPos grappleProj = return $ grappleProj
    { P._hitbox = const $ lineHitbox startPos endPos
    , P._update = if
        | reachedTarget -> return . id
        | otherwise     -> updateGrappleProjExtend targetPos
    }
    where
        cfg           = _config (P._data grappleProj :: GrappleProjData)
        startPos      = grappleProjStartPos grappleProj
        endPos        = extendGrappleProjEndPos (startPos, grappleProjEndPos grappleProj) targetPos cfg
        reachedTarget = vecDistSq targetPos endPos <= _projThrowTargetDistSq cfg
