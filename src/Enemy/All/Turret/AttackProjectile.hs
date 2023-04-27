module Enemy.All.Turret.AttackProjectile
    ( mkTurretAttackProjectile
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))
import qualified Data.List as L
import qualified Data.Set as S

import Attack.Hit
import Collision
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Turret
import Configs.All.Settings
import Configs.All.Settings.Debug
import Enemy.All.Turret.Data
import Enemy.All.Turret.Sprites
import Id
import Msg
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

hasHitboxFrameTagName = "hasHitbox"       :: FrameTagName
debugHbxColor         = Color 0 0 255 155 :: Color

seekingWallModeRegisteredCollisions = S.fromList
    [ ProjRegisteredSurfaceCollision
    ] :: S.Set ProjectileRegisteredCollision

firingModeRegisteredCollisions = S.fromList
    [ ProjRegisteredPlayerCollision
    ] :: S.Set ProjectileRegisteredCollision

data TurretEnemyStatus
    = TurretEnemyAlive
    | TurretEnemyDead
    | TurretEnemyInStasis
    deriving Eq

data TurretAttackProjMode
    = SeekingWallMode
    | FiringMode

data TurretAttackProjData = TurretAttackProjData
    { _mode              :: TurretAttackProjMode
    , _pos               :: Pos2
    , _dir               :: Direction
    , _width             :: Float
    , _turretEnemyId     :: MsgId
    , _turretEnemyStatus :: TurretEnemyStatus
    , _beamMidSpr        :: Sprite
    , _beamEndSpr        :: Sprite
    , _config            :: TurretEnemyConfig
    }

mkTurretAttackProjectile :: MonadIO m => Pos2 -> Direction -> MsgId -> TurretEnemyData -> m (Some Projectile)
mkTurretAttackProjectile pos dir turretEnemyId turretData =
    let
        dummyHbx = dummyHitbox pos
        cfg      = _turret $ _config (turretData :: TurretEnemyData)
        sprs     = _sprites turretData

        atkProjData = TurretAttackProjData
            { _mode              = SeekingWallMode
            , _pos               = pos
            , _dir               = dir
            , _width             = _attackMaxRangeX cfg
            , _turretEnemyId     = turretEnemyId
            , _turretEnemyStatus = TurretEnemyAlive
            , _beamMidSpr        = _attackBeamMid sprs
            , _beamEndSpr        = _attackBeamEnd sprs
            , _config            = cfg
            }
    in do
        msgId <- newId
        return . Some $ (mkProjectile atkProjData msgId dummyHbx maxSecs)
            { _hitbox               = atkProjHitbox
            , _registeredCollisions = seekingWallModeRegisteredCollisions
            , _update               = updateAtkProj
            , _draw                 = drawAtkProj
            , _processCollisions    = processAtkProjCollisions
            , _voluntaryClear       = voluntaryClearData
            }

isTurretEnemyInStasis :: Projectile TurretAttackProjData -> Bool
isTurretEnemyInStasis = (== TurretEnemyInStasis) . _turretEnemyStatus . _data

atkProjHitbox :: ProjectileHitbox TurretAttackProjData
atkProjHitbox atkProj = case _mode atkProjData of
    SeekingWallMode ->
        let height = spriteImageHeight $ _beamMidSpr atkProjData
        in rectHitbox pos width height
    FiringMode
        | hasHitbox && not (isTurretEnemyInStasis atkProj) ->
            let
                sprImgHeight = spriteImageHeight beamMidSpr
                y'           = y - sprImgHeight / 2.0
                pos'         = Pos2 x' y'
            in rectHitbox pos' width sprImgHeight
        | otherwise                                        -> dummyHitbox pos
    where
        atkProjData = _data atkProj
        beamMidSpr  = _beamMidSpr atkProjData
        hasHitbox   = hasHitboxFrameTagName `isSpriteFrameTag` beamMidSpr
        Pos2 x y    = _pos (atkProjData   :: TurretAttackProjData)
        dir         = _dir (atkProjData   :: TurretAttackProjData)
        width       = _width (atkProjData :: TurretAttackProjData)

        x'
            | dir == LeftDir = x - width
            | otherwise      = x
        pos                  = Pos2 x' y

updateAtkProj :: forall m. MsgsRead UpdateProjectileMsgsPhase m => ProjectileUpdate TurretAttackProjData m
updateAtkProj atkProj = readTurretEnemyStatus <&> \case
    TurretEnemyDead -> atkProj
        { _data = atkProjData {_turretEnemyStatus = TurretEnemyDead}
        , _ttl  = 0.0
        }

    TurretEnemyInStasis -> atkProj
        { _data = atkProjData {_turretEnemyStatus = TurretEnemyInStasis}
        }

    TurretEnemyAlive -> case _mode atkProjData of
        SeekingWallMode -> atkProj
        FiringMode      ->
            let
                beamMidSpr = updateSprite $ _beamMidSpr atkProjData
                beamEndSpr = updateSprite $ _beamEndSpr atkProjData
                ttl        = if
                    | spriteFinished beamMidSpr -> 0.0
                    | otherwise                 -> _ttl atkProj
            in atkProj
                { _data = atkProjData
                    { _turretEnemyStatus = TurretEnemyAlive
                    , _beamMidSpr        = beamMidSpr
                    , _beamEndSpr        = beamEndSpr
                    }
                , _ttl  = ttl
                }

    where
        atkProjData = _data atkProj

        readTurretEnemyStatus :: m TurretEnemyStatus
        readTurretEnemyStatus = L.foldl' processMsg TurretEnemyDead <$> readMsgs
            where
                processMsg :: TurretEnemyStatus -> InfoMsgPayload -> TurretEnemyStatus
                processMsg status p = case p of
                    InfoMsgEnemyPos _ msgId
                        | msgId == turretEnemyId && status /= TurretEnemyInStasis -> TurretEnemyAlive
                    InfoMsgEnemyInStasis msgId
                        | msgId == turretEnemyId                                  -> TurretEnemyInStasis
                    _                                                             -> status
                    where turretEnemyId = _turretEnemyId atkProjData

drawAtkProj :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => ProjectileDraw TurretAttackProjData m
drawAtkProj atkProj = case _mode atkProjData of
    SeekingWallMode -> return ()

    FiringMode ->
        let
            hbx        = (P._hitbox atkProj) atkProj
            hbxLeft    = hitboxLeft hbx
            hbxCenterY = vecY $ hitboxCenter hbx
            dir        = _dir (atkProjData :: TurretAttackProjData)
            width      = _width (atkProjData :: TurretAttackProjData)

            atkMouthMidBeamOffsetX = _attackMouthMidBeamOffsetX $ _config (atkProjData :: TurretAttackProjData)
            midPosX                = case dir of
                LeftDir  -> hbxLeft
                RightDir -> hbxLeft + atkMouthMidBeamOffsetX
            midPos                 = Pos2 midPosX hbxCenterY
            midWidth               = width - atkMouthMidBeamOffsetX
            beamMidSpr             = _beamMidSpr atkProjData
            midSprImgHeight        = spriteImageHeight beamMidSpr
        in do
            whenM (readSettingsConfig _debug _drawEntityHitboxes) $
                drawHitbox debugHbxColor debugHitboxZIndex hbx

            when (isTurretEnemyInStasis atkProj) $
                setGraphicsBlendMode BlendModeAdditive

            when (midWidth > 0.0) $
                drawSpriteRect midPos midWidth midSprImgHeight enemyAttackProjectileZIndex beamMidSpr

            when (isTurretEnemyInStasis atkProj) $
                setGraphicsBlendMode BlendModeAlpha

    where atkProjData = _data atkProj

processAtkProjCollisions :: ProjectileProcessCollisions TurretAttackProjData
processAtkProjCollisions collisions atkProj = case _mode atkProjData of
    SeekingWallMode ->
        let
            processSurfaceCollision :: ProjectileCollision -> Distance -> Distance
            processSurfaceCollision collision !closestCollisionDist = case collision of
                ProjSurfaceCollision surfaceHbx _ ->
                    let
                        leftDist  = abs $ hitboxLeft surfaceHbx - x
                        rightDist = abs $ hitboxRight surfaceHbx - x
                    in L.minimum [leftDist, rightDist, closestCollisionDist]
                _                                 -> closestCollisionDist

            width     = _width (atkProjData :: TurretAttackProjData)
            width'    = foldr processSurfaceCollision width collisions
            x         = vecX $ _pos (atkProjData :: TurretAttackProjData)
            atkProjId = P._msgId atkProj

            update = \p -> p
                { _data                 = (P._data p)
                    { _mode  = FiringMode
                    , _width = width'
                    }
                , _registeredCollisions = firingModeRegisteredCollisions
                }
        in [mkMsgTo (ProjectileMsgUpdate update) atkProjId]

    FiringMode ->
        let
            processPlayerCollision
                :: ProjectileCollision
                -> [Msg ThinkCollisionMsgsPhase]
                -> [Msg ThinkCollisionMsgsPhase]
            processPlayerCollision collision !msgs = case collision of
                ProjPlayerCollision player ->
                    let
                        atkHbx          = (P._hitbox atkProj) atkProj
                        playerHbx       = collisionEntityHitbox player
                        atkIntersectPos = hitboxAvgIntersectPos atkHbx playerHbx
                        atkProjId       = P._msgId atkProj
                        atkHit          = (mkAttackHitEmpty atkProjId atkIntersectPos)
                            { _damage   = _attackDamage $ _config (atkProjData :: TurretAttackProjData)
                            , _isRanged = True
                            }
                        playerId        = collisionEntityMsgId player
                        collisionMsg    = mkMsgTo (HurtMsgAttackHit atkHit) playerId
                    in collisionMsg:msgs
                _                          -> msgs
        in foldr processPlayerCollision [] collisions

    where atkProjData = _data atkProj

voluntaryClearData :: ProjectileVoluntaryClear TurretAttackProjData
voluntaryClearData atkProj = case spriteImage (_beamEndSpr atkProjData) of
    Nothing  -> Nothing
    Just img -> Just $ ProjectileVoluntaryClearData
        { _pos    = _pos (atkProjData :: TurretAttackProjData)
        , _dir    = _dir (atkProjData :: TurretAttackProjData)
        , _zIndex = enemyAttackProjectileZIndex
        , _image  = img
        }
    where atkProjData = _data atkProj
