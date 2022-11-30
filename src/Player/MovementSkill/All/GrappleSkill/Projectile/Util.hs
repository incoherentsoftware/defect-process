module Player.MovementSkill.All.GrappleSkill.Projectile.Util
    ( grappleProjStartPos
    , grappleProjEndPos
    , playerTowardsVel
    , grappleProjSurfaceEffectMsg
    , grappleProjHitEffectMsgs
    , drawGrappleProj
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack.Hit
import Attack.Util
import Collision
import Configs.All.PlayerSkill.Grapple
import FileCache
import Id
import Msg
import Particle
import Particle.All.AttackSpecks
import Particle.All.Simple
import Player.MovementSkill.All.GrappleSkill.Data
import Player.MovementSkill.All.GrappleSkill.Projectile.Data
import Projectile
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

packPath          = \f -> PackResourceFilePath "data/player/player-skills.pack" f
surfaceEffectPath = packPath "grapple-surface-effect.spr" :: PackResourceFilePath
hitEffectPath     = packPath "grapple-hit-effect.spr"     :: PackResourceFilePath

grappleProjStartPos :: Projectile GrappleProjData -> Pos2
grappleProjStartPos = hitboxStartVertex . projectileHitbox

grappleProjEndPos :: Projectile GrappleProjData -> Pos2
grappleProjEndPos = last . hitboxVertices . projectileHitbox

playerTowardsVel :: Pos2 -> Pos2 -> GrappleConfig -> Vel2
playerTowardsVel startPos intersectPos cfg = playerVel
    where
        playerVec = toVec2 . vecNormalize $ intersectPos `vecSub` startPos
        playerVel = toVel2 $ playerVec `vecMul` _playerGrappleSpeed cfg

grappleProjSurfaceEffectMsg :: Pos2 -> Hitbox -> Msg ThinkCollisionMsgsPhase
grappleProjSurfaceEffectMsg intersectPos collisionHbx = mkMsg $ ParticleMsgAddM mkSurfaceEffect
    where
        (effectDir, effectAngle) = particleClosestDirAngle intersectPos collisionHbx
        mkSurfaceEffect          =
            loadSimpleParticleRotated intersectPos effectDir worldEffectZIndex effectAngle surfaceEffectPath

grappleProjHitEffectMsgs :: Pos2 -> [Msg ThinkCollisionMsgsPhase]
grappleProjHitEffectMsgs intersectPos =
    [ mkMsg $ ParticleMsgAddM mkHitEffect
    , mkMsg $ ParticleMsgAddM (mkAttackSpecksParticle grappleFakeAtkHit)
    ]
        where
            mkHitEffect       = loadSimpleParticle intersectPos RightDir worldEffectZIndex hitEffectPath
            grappleFakeAtkHit = (mkAttackHitEmpty NullId intersectPos)
                { _hitEffectType   = NormalHitEffect
                , _specksType      = Just GrappleSpecksType
                , _specksPos       = Just SpecksAtkIntersectPos
                , _specksDirection = Just SpecksAnyDir
                }

drawGrappleProj :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw GrappleProjData m
drawGrappleProj grappleProj = case hitboxVertices (projectileHitbox grappleProj) of
    [Pos2 startX startY, endPos@(Pos2 endX endY)] ->
        let
            grappleProjData     = P._data grappleProj
            startVisualPos      = (_startVisualPos grappleProjData) grappleProj
            vel                 = _startPosVel (grappleProjData :: GrappleProjData)
            prongsVisualAngle   = atan2 (endY - startY) (endX - startX)
            prongsLineOffset    = _projProngsLineOffset $ _config (grappleProjData :: GrappleProjData)
            lineEndVisualOffset = prongsLineOffset `vecRotate` prongsVisualAngle
            lineEndVisualPos    = endPos `vecAdd` lineEndVisualOffset

            images    = _images (grappleProjData :: GrappleProjData)
            cableImg  = _cable images
            prongsImg = _prongs images
        in do
            startVisualPos' <- graphicsLerpPos startVisualPos vel
            let
                lineVisualDiffY  = vecY lineEndVisualPos - vecY startVisualPos'
                lineVisualDiffX  = vecX lineEndVisualPos - vecX startVisualPos'
                lineVisualAngle  = atan2 lineVisualDiffY lineVisualDiffX
                lineVisualLength = maxZero $ vecDist startVisualPos' lineEndVisualPos
                lineHeight       = imageHeight cableImg
                zIndex           = playerOverSpecialLegsZIndex
            drawImageRectRotated startVisualPos' lineVisualLength lineHeight zIndex lineVisualAngle cableImg

            drawImageRotated endPos RightDir zIndex prongsVisualAngle prongsImg

    _ -> return ()
