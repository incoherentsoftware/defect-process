module Player.Gun.All.GrenadeLauncher.Grenade
    ( mkGrenade
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.List as L
import qualified Data.Set as S

import Attack
import Attack.Projectile
import Collision
import Configs
import Configs.All.PlayerGun
import Configs.All.PlayerGun.GrenadeLauncher
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import FileCache
import Id
import InfoMsg.Util
import Msg
import Player
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

packPath       = \p -> PackResourceFilePath "data/player/player-guns.pack" p
projectilePath = packPath "grenade-launcher-projectile.image"         :: PackResourceFilePath
explosionPath  = packPath "grenade-launcher-projectile-explosion.atk" :: PackResourceFilePath

debugHitboxColor = Color 0 155 30 155 :: Color

data GrenadeData = GrenadeData
    { _playerHitbox     :: Hitbox
    , _explosionAtkDesc :: AttackDescription
    , _config           :: GrenadeLauncherConfig
    }

mkGrenadeData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Player -> m GrenadeData
mkGrenadeData player =
    GrenadeData <$>
    pure (playerHitbox player) <*>
    loadPackAttackDescription explosionPath <*>
    readConfig _playerGun _grenadeLauncher

mkGrenade :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Player -> m (Some Projectile)
mkGrenade player = do
    grenadeData <- mkGrenadeData player

    let
        plId            = _msgId (player :: Player)
        cfg             = _config (grenadeData :: GrenadeData)
        shotStartOffset = Pos2 0.0 (_shotStartShoulderOffsetY cfg)
        grenadeWidth    = _grenadeWidth cfg
        grenadeHeight   = _grenadeHeight cfg
        centeredOffset  = Pos2 (grenadeWidth / 2.0) (grenadeHeight / 2.0)
        pos             = playerShoulderPos player `vecAdd` shotStartOffset `vecSub` centeredOffset
        hbx             = rectHitbox pos grenadeWidth grenadeHeight
        targetPos       = playerAimTarget player (_shootRange cfg)
        normProjVec     = toVec2 . vecNormalize $ targetPos `vecSub` pos
        projVel         = toVel2 $ normProjVec `vecMul` _grenadeSpeed cfg
        shotAliveSecs   = _shotAliveSecs cfg

    msgId      <- newId
    grenadeImg <- loadPackImage projectilePath

    return . Some $ (mkProjectile grenadeData msgId hbx shotAliveSecs)
        { _vel                  = projVel
        , _ownerId              = plId
        , _registeredCollisions = S.fromList
            [ ProjRegisteredSurfaceCollision
            , ProjRegisteredEnemyCollision
            , ProjRegisteredRoomItemCollision
            ]
        , _update               = updateGrenade
        , _draw                 = drawGrenade grenadeImg
        , _processCollisions    = processGrenadeCollisions
        }

updateGrenade :: MsgsRead UpdateProjectileMsgsPhase m => ProjectileUpdate GrenadeData m
updateGrenade grenade =
    let
        processMsg :: Projectile GrenadeData -> InfoMsgPayload -> Projectile GrenadeData
        processMsg g! m = case m of
            InfoMsgPlayer playerInfo -> g
                { _data = (_data g)
                    { _playerHitbox = _hitbox (playerInfo :: PlayerInfo)
                    }
                }
            _                        -> g
    in
        L.foldl' processMsg grenade <$> readMsgs >>=
        return . grenadeUpdatePosVel

grenadeUpdatePosVel :: Projectile GrenadeData -> Projectile GrenadeData
grenadeUpdatePosVel grenade = grenade
    { _hitbox = const hitbox'
    , _vel    = toVel2 velVec'
    }
    where
        hitbox  = projectileHitbox grenade
        posVec  = toVec2 $ hitboxTopLeft hitbox
        velVec  = toVec2 $ P._vel grenade
        posVec' = posVec `vecAdd` (velVec `vecMul` timeStep)
        hitbox' = setHitboxTopLeft (toPos2 posVec') hitbox
        cfg     = _config (P._data grenade :: GrenadeData)
        velVec' = velVec `vecAdd` (Vec2 0.0 (_grenadeGravity cfg * timeStep))

processGrenadeCollisions :: ProjectileProcessCollisions GrenadeData
processGrenadeCollisions collisions grenade = case collisions of
    []            -> []
    (collision:_) -> case collision of
        ProjSurfaceCollision _ _ -> grenadeCollision grenade
        ProjEnemyCollision _     -> grenadeCollision grenade
        ProjRoomItemCollision _  -> grenadeCollision grenade
        _                        -> []

grenadeCollision :: Projectile GrenadeData -> [Msg ThinkCollisionMsgsPhase]
grenadeCollision grenade =
    [ mkMsgTo (ProjectileMsgSetTtl 0.0) grenadeId
    , mkMsg $ NewUpdateProjectileMsgAddM mkExplosion
    ] ++ playerVelMsgs
        where
            grenadeId        = P._msgId grenade
            grenadeCenterPos = hitboxCenter $ projectileHitbox grenade
            grenadeData      = P._data grenade
            explosionAtkDesc = _explosionAtkDesc grenadeData
            mkExplosion      = mkPlayerAttackProjectile grenadeCenterPos RightDir explosionAtkDesc

            playerHbx               = _playerHitbox grenadeData
            playerHbxCenter         = hitboxCenter playerHbx
            cfg                     = _config (grenadeData :: GrenadeData)
            explosionLaunchRadiusSq = _grenadeExplosionLaunchRadius cfg ** 2

            playerVelMsgs
                | vecDistSq grenadeCenterPos playerHbxCenter <= explosionLaunchRadiusSq =
                    let
                        normVelVec = toVec2 . vecNormalize $ playerHbxCenter `vecSub` grenadeCenterPos
                        vel        = toVel2 $ normVelVec `vecMul` _grenadeExplosionLaunchSpeed cfg
                    in [mkMsgEx (PlayerMsgSetVelocity vel) MsgAfterNormalOrder]
                | otherwise                                                             = []

drawGrenade :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => Image -> ProjectileDraw GrenadeData m
drawGrenade img grenade =
    let
        hbx            = projectileHitbox grenade
        pos            = hitboxCenter hbx
        Vel2 velX velY = P._vel grenade
        angle          = atan2 velY velX
    in do
        drawImageRotated pos RightDir worldProjectileZIndex angle img
        whenM (readSettingsConfig _debug _drawEntityHitboxes) $
            drawHitbox debugHitboxColor debugHitboxZIndex hbx
