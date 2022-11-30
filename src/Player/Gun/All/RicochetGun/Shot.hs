module Player.Gun.All.RicochetGun.Shot
    ( mkShotProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))
import qualified Data.Set as S

import Attack.Hit
import Attack.Util
import Collision
import Configs
import Configs.All.PlayerGun
import Configs.All.PlayerGun.RicochetGun
import Enemy as E
import FileCache
import Id
import Msg
import Particle
import Particle.All.AttackSpecks.Types
import Particle.All.Simple
import Player
import Player.Gun.All.RicochetGun.Shot.Data
import Player.Gun.All.RicochetGun.Util
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

gunsPack       = \p -> PackResourceFilePath "data/player/player-guns.pack" p
missEffectPath = gunsPack "ricochet-gun-projectile-miss-effect.spr" :: PackResourceFilePath
hitEffectPath  = gunsPack "ricochet-gun-projectile-hit-effect.spr"  :: PackResourceFilePath

noBounceSoundPath    = "event:/SFX Events/Player/Guns/ricochet-gun-no-bounce"    :: FilePath
bounceSoundPath      = "event:/SFX Events/Player/Guns/ricochet-gun-bounce"       :: FilePath
bounceEnemySoundPath = "event:/SFX Events/Player/Guns/ricochet-gun-bounce-enemy" :: FilePath

mkShotProjectile :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Player -> Int -> m (Some Projectile)
mkShotProjectile player numBounces = do
    cfg <- readConfig _playerGun _ricochetGun
    let
        startPos  = playerShoulderPos player
        targetPos = playerAimTarget player (_shootRange cfg)
        playerId  = _msgId (player :: Player)
    mkShotProjectileInternal DirectShot startPos targetPos numBounces playerId NoPrevBounceData

mkShotProjectileInternal
    :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m)
    => ShotType
    -> Pos2
    -> Pos2
    -> Int
    -> MsgId
    -> ShotPrevBounceData
    -> m (Some Projectile)
mkShotProjectileInternal shotType startPos targetPos numBounces playerId prevBounceData = do
    shotData <- mkShotProjectileData shotType numBounces prevBounceData
    msgId    <- newId
    let hbx   = lineHitbox startPos targetPos

    return . Some $ (mkProjectile shotData msgId hbx maxSecs)
        { _ownerId              = playerId
        , _think                = thinkShot
        , _update               = updateShot
        , _draw                 = drawShotLine
        , _processCollisions    = processShotCollisions
        , _registeredCollisions = S.fromList
            [ ProjRegisteredEnemyCollision
            , ProjRegisteredSurfaceCollision
            , ProjRegisteredRoomItemCollision
            ]
        }

thinkShot :: (MonadIO m, MsgsRead ThinkProjectileMsgsPhase m) => ProjectileThink ShotProjectileData m
thinkShot shot
    | _frameIndex beamSpr == 0 && _frameChanged beamSpr = readShotBounceEnemyPositions' <&> \case
        (_:_)
            | numBounces > 0 -> []
        _                    -> [mkMsg $ AudioMsgPlaySound noBounceSoundPath shotEndPos]

    | spriteFinished beamSpr = do
        bounceMsgs <- if
            | numBounces <= 0 -> return []
            | otherwise       -> case _lastHit shotData of
                NoHit      -> return []
                SurfaceHit -> readShotBounceEnemyPositions' <&> \case
                    (enemyPos:_) -> mkShotBounceToEnemyProjMsgs enemyPos
                    []           -> mkShotBounceReflectOffSurfaceProjMsgs
                EnemyHit   -> readShotBounceEnemyPositions' <&> \case
                    (enemyPos:_) -> mkShotBounceToEnemyProjMsgs enemyPos
                    []           -> mkShotBounceToSurfaceProjMsgs

        let removeShotMsg = mkMsgTo (ProjectileMsgSetTtl 0.0) (P._msgId shot)
        return $ removeShotMsg:bounceMsgs

    | otherwise = return []

    where
        shotData       = P._data shot
        beamSpr        = _beamSprite shotData
        shotHbx        = projectileHitbox shot
        shotEndPos     = hitboxEndVertex shotHbx
        numBounces     = _numBounces shotData
        prevBounceData = _prevBounceData shotData
        cfg            = _config (P._data shot :: ShotProjectileData)

        readShotBounceEnemyPositions' = readShotBounceEnemyPositions shotEndPos prevBounceData cfg

        mkShotProjectileInternal' = \targetPos -> mkShotProjectileInternal
            BounceShot
            shotEndPos
            targetPos
            (numBounces - 1)
            (P._ownerId shot)
            prevBounceData

        mkShotBounceToEnemyProjMsgs = \targetEnemyPos ->
            let
                angleVec     = vecNormalize $ targetEnemyPos `vecSub` shotEndPos
                bounceOffset = angleVec `vecMul` _shootRange cfg
                targetPos    = shotEndPos `vecAdd` bounceOffset
            in
                [ mkMsg $ NewUpdateProjectileMsgAddM (mkShotProjectileInternal' targetPos)
                , mkMsg $ AudioMsgPlaySound bounceEnemySoundPath shotEndPos
                ]

        mkShotBounceReflectOffSurfaceProjMsgs =
            let targetPos = shotBounceReflectOffSurfaceTargetPos shotHbx prevBounceData cfg
            in
                [ mkMsg $ NewUpdateProjectileMsgAddM (mkShotProjectileInternal' targetPos)
                , mkMsg $ AudioMsgPlaySound bounceSoundPath shotEndPos
                ]

        mkShotBounceToSurfaceProjMsgs =
            let targetPos = shotBounceToSurfaceTargetPos shotHbx cfg
            in
                [ mkMsg $ NewUpdateProjectileMsgAddM (mkShotProjectileInternal' targetPos)
                , mkMsg $ AudioMsgPlaySound bounceSoundPath shotEndPos
                ]

updateShot :: Monad m => ProjectileUpdate ShotProjectileData m
updateShot shot = return $ shot
    { P._data = shotData {_beamSprite = updateSprite beamSpr}
    }
    where
        shotData = P._data shot
        beamSpr  = _beamSprite shotData

hitboxApproxEq :: Hitbox -> Hitbox -> Bool
hitboxApproxEq hbx1 hbx2 = hitboxCenter hbx1 `pos2ApproxEq` hitboxCenter hbx2
    where pos2ApproxEq = \(Pos2 x1 y1) (Pos2 x2 y2) -> x1 `approxEq` x2 && y1 `approxEq` y2

processShotCollisions :: ProjectileProcessCollisions ShotProjectileData
processShotCollisions projCollisions shot = processCollisions $ sortProjectileCollisions projCollisions shot
    where
        processCollisions :: [(Pos2, ProjectileCollision)] -> [Msg ThinkCollisionMsgsPhase]
        processCollisions []                                     = []
        processCollisions ((intersectPos, collision):collisions) = case collision of
            -- shot stops at first thing hit
            ProjEnemyCollision enemy
                | not (isPrevBounceEnemyId enemy)  -> shotEntityCollision enemy shot intersectPos
            ProjSurfaceCollision hbx _
                | not (isPrevBounceSurfaceHbx hbx) -> shotSurfaceCollision hbx shot intersectPos
            ProjRoomItemCollision (Some roomItem)  -> shotEntityCollision roomItem shot intersectPos
            _                                      -> processCollisions collisions
            where
                prevBounceData = _prevBounceData $ P._data shot

                isPrevBounceEnemyId :: forall d. Enemy d -> Bool
                isPrevBounceEnemyId enemy = case prevBounceData of
                    PrevBounceEnemyId enemyId -> E._msgId enemy == enemyId
                    _                         -> False

                isPrevBounceSurfaceHbx = \hbx -> case prevBounceData of
                    PrevBounceSurfaceHitbox surfaceHbx -> hbx `hitboxApproxEq` surfaceHbx
                    _                                  -> False

setShotData :: ShotLastHit -> ShotPrevBounceData -> Projectile ShotProjectileData -> Projectile ShotProjectileData
setShotData lastHit prevBounceData shot = shot
    { _data = (P._data shot)
        { _lastHit        = lastHit
        , _prevBounceData = prevBounceData
        }
    }

shotSurfaceCollision :: Hitbox -> Projectile ShotProjectileData -> Pos2 -> [Msg ThinkCollisionMsgsPhase]
shotSurfaceCollision surfaceHbx shot intersectPos =
    [ mkMsgTo (ProjectileMsgSetHitbox hbx) shotId
    , mkMsgTo ProjectileMsgRemoveCollision shotId
    , mkMsgTo (ProjectileMsgUpdate $ setShotData SurfaceHit prevBounceData) shotId
    , mkMsg $ ParticleMsgAddM mkMissEffect
    ]
        where
            shotId         = P._msgId shot
            startPos       = hitboxStartVertex $ projectileHitbox shot
            hbx            = lineHitbox startPos intersectPos
            prevBounceData = PrevBounceSurfaceHitbox surfaceHbx

            (effectDir, effectAngle) = particleClosestDirAngle intersectPos surfaceHbx
            mkMissEffect             =
                loadSimpleParticleRotated intersectPos effectDir worldEffectZIndex effectAngle missEffectPath

shotEntityCollision :: CollisionEntity e => e -> Projectile ShotProjectileData -> Pos2 -> [Msg ThinkCollisionMsgsPhase]
shotEntityCollision entity shot intersectPos =
    [ mkMsgTo (ProjectileMsgSetHitbox hbx) shotId
    , mkMsgTo ProjectileMsgRemoveCollision shotId
    , mkMsgTo (ProjectileMsgUpdate $ setShotData EnemyHit prevBounceData) shotId
    , mkMsg $ ParticleMsgAddM mkImpactEffect
    , mkMsgTo (HurtMsgAttackHit shotHit) entityId
    ]
    where
        shotData       = P._data shot
        shotId         = P._msgId shot
        startPos       = hitboxStartVertex $ projectileHitbox shot
        hbx            = lineHitbox startPos intersectPos
        entityId       = collisionEntityMsgId entity
        entityHbx      = collisionEntityHitbox entity
        prevBounceData = PrevBounceEnemyId entityId

        (effectDir, effectAngle) = particleClosestDirAngle intersectPos entityHbx
        mkImpactEffect           =
            loadSimpleParticleRotated intersectPos effectDir worldEffectZIndex effectAngle hitEffectPath

        cfg     = _config (shotData :: ShotProjectileData)
        shotHit = (mkAttackHitEmpty shotId intersectPos)
            { _vel               = _shotHitVel cfg
            , _damage            = _shotDamage cfg
            , _stagger           = _shotStagger cfg
            , _hitstunMultiplier = _shotHitstunMultiplier cfg
            , _isRanged          = True
            , _hitEffectType     = WeakHitEffect
            , _specksType        = Just RicochetSpecksType
            , _specksDirection   = Just SpecksAnyDir
            }

drawShotLine :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => ProjectileDraw ShotProjectileData m
drawShotLine shot = case hitboxVertices (projectileHitbox shot) of
    [startPos@(Pos2 startX _), endPos@(Pos2 endX _)]
        | Just beamImg <- spriteImage beamSpr ->
            let
                startVisualOffset = case _type (shotData :: ShotProjectileData) of
                    BounceShot -> zeroPos2
                    DirectShot ->
                        let
                            cfg                  = _config (shotData :: ShotProjectileData)
                            dir
                                | endX >= startX = RightDir
                                | otherwise      = LeftDir
                        in _handsOffset cfg `vecFlip` dir

                startVisualPos   = startPos `vecAdd` startVisualOffset
                lineVisualDiffY  = vecY endPos - vecY startVisualPos
                lineVisualDiffX  = vecX endPos - vecX startVisualPos
                lineVisualAngle  = atan2 lineVisualDiffY lineVisualDiffX
                lineVisualLength = maxZero $ vecDist startVisualPos endPos
                lineHeight       = imageHeight beamImg
                zIndex           = playerOverSpecialLegsZIndex
            in drawImageRectRotated startVisualPos lineVisualLength lineHeight zIndex lineVisualAngle beamImg

    _ -> return ()

    where
        shotData = P._data shot
        beamSpr  = _beamSprite shotData
