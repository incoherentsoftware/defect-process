module Enemy.All.Boss.TurretProjectile
    ( mkTurretProjectile
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.List as L
import qualified Data.Set as S

import Attack.Hit
import Collision
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Boss
import Configs.All.Settings
import Configs.All.Settings.Debug
import Enemy.All.Boss.Data
import FileCache
import Id
import Msg
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

hasHitboxFrameTagName = "hasHitbox"       :: FrameTagName
debugHbxColor         = Color 0 0 255 155 :: Color

projectileSprPath =
    PackResourceFilePath "data/enemies/boss-enemy-attack1.pack" "attack-turret-projectile.spr" :: PackResourceFilePath

seekingWallModeRegisteredCollisions = S.fromList
    [ ProjRegisteredSurfaceCollision
    ] :: S.Set ProjectileRegisteredCollision

firingModeRegisteredCollisions = S.fromList
    [ ProjRegisteredPlayerCollision
    ] :: S.Set ProjectileRegisteredCollision

data TurretProjectileMode
    = SeekingWallMode
    | FiringMode

data TurretProjectileData = TurretProjectileData
    { _mode       :: TurretProjectileMode
    , _pos        :: Pos2
    , _dir        :: Direction
    , _width      :: Float
    , _beamMidSpr :: Sprite
    , _config     :: BossEnemyConfig
    }

mkTurretProjectile
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => Pos2
    -> Direction
    -> BossEnemyData
    -> m (Some Projectile)
mkTurretProjectile pos dir bossEnemyData = do
    let
        dummyHbx = dummyHitbox pos
        cfg      = _boss $ _config (bossEnemyData :: BossEnemyData)

    spr <- loadPackSprite projectileSprPath

    let
        atkProjData = TurretProjectileData
            { _mode       = SeekingWallMode
            , _pos        = pos
            , _dir        = dir
            , _width      = _turretProjMaxRange cfg
            , _beamMidSpr = spr
            , _config     = cfg
            }

    msgId <- newId
    return . Some $ (mkProjectile atkProjData msgId dummyHbx maxSecs)
        { _hitbox               = atkProjHitbox
        , _registeredCollisions = seekingWallModeRegisteredCollisions
        , _update               = updateAtkProj
        , _draw                 = drawAtkProj
        , _processCollisions    = processAtkProjCollisions
        }

atkProjHitbox :: ProjectileHitbox TurretProjectileData
atkProjHitbox atkProj = case _mode atkProjData of
    SeekingWallMode ->
        let height = spriteImageHeight $ _beamMidSpr atkProjData
        in rectHitbox pos width height
    FiringMode
        | hasHitbox ->
            let
                sprImgHeight = spriteImageHeight beamMidSpr
                y'           = y - sprImgHeight / 2.0
                pos'         = Pos2 x' y'
            in rectHitbox pos' width sprImgHeight
        | otherwise -> dummyHitbox pos
    where
        atkProjData = _data atkProj
        beamMidSpr  = _beamMidSpr atkProjData
        hasHitbox   = hasHitboxFrameTagName `isSpriteFrameTag` beamMidSpr
        Pos2 x y    = _pos (atkProjData :: TurretProjectileData)
        dir         = _dir (atkProjData :: TurretProjectileData)
        width       = _width (atkProjData :: TurretProjectileData)

        x'
            | dir == LeftDir = x - width
            | otherwise      = x
        pos                  = Pos2 x' y

updateAtkProj :: Monad m => ProjectileUpdate TurretProjectileData m
updateAtkProj atkProj = return $ case _mode atkProjData of
    SeekingWallMode -> atkProj
    FiringMode      ->
        let
            beamMidSpr   = updateSprite $ _beamMidSpr atkProjData
            atkProjData' = atkProjData
                { _beamMidSpr = beamMidSpr
                }

            ttl                             = _ttl atkProj
            ttl'
                | spriteFinished beamMidSpr = 0.0
                | otherwise                 = ttl
        in atkProj
            { _data = atkProjData'
            , _ttl  = ttl'
            }
    where atkProjData = _data atkProj

processAtkProjCollisions :: ProjectileProcessCollisions TurretProjectileData
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

            width     = _width (atkProjData :: TurretProjectileData)
            width'    = foldr processSurfaceCollision width collisions
            x         = vecX $ _pos (atkProjData :: TurretProjectileData)
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
                        cfg             = _config (atkProjData :: TurretProjectileData)
                        atkHit          = (mkAttackHitEmpty atkProjId atkIntersectPos)
                            { _damage   = _turretProjDamage cfg
                            , _isRanged = True
                            } :: AttackHit
                        playerId        = collisionEntityMsgId player
                    in
                        [ mkMsgTo (HurtMsgAttackHit atkHit) playerId
                        , mkMsg $ WorldMsgScreenshake (_turretScreenshakeMagnitude cfg)
                        ] ++ msgs
                _                          -> msgs
        in foldr processPlayerCollision [] collisions

    where atkProjData = _data atkProj

drawAtkProj :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => ProjectileDraw TurretProjectileData m
drawAtkProj atkProj = case _mode atkProjData of
    SeekingWallMode -> return ()

    FiringMode ->
        let
            hbx             = (P._hitbox atkProj) atkProj
            hbxLeft         = hitboxLeft hbx
            hbxCenterY      = vecY $ hitboxCenter hbx
            width           = _width (atkProjData :: TurretProjectileData)
            midPos          = Pos2 hbxLeft hbxCenterY
            midWidth        = width
            beamMidSpr      = _beamMidSpr atkProjData
            midSprImgHeight = spriteImageHeight beamMidSpr
        in do
            whenM (readSettingsConfig _debug _drawEntityHitboxes) $
                drawHitbox debugHbxColor debugHitboxZIndex hbx

            when (midWidth > 0.0) $
                drawSpriteRect midPos midWidth midSprImgHeight bossUnderBodyZIndex beamMidSpr

    where atkProjData = _data atkProj
