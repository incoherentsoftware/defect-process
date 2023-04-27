module Player.Gun.All.GrenadeLauncher.Mine
    ( mkMine
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import Attack
import Collision
import Configs
import Configs.All.PlayerGun
import Configs.All.PlayerGun.GrenadeLauncher
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import Enemy as E
import FileCache
import Id
import Msg
import Player
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

packPath              = \p -> PackResourceFilePath "data/player/player-guns.pack" p
mineOffImagePath      = packPath "grenade-launcher-mine-off.image"     :: PackResourceFilePath
mineOnImagePath       = packPath "grenade-launcher-mine-on.image"      :: PackResourceFilePath
overlayImagePath      = packPath "grenade-launcher-mine-overlay.image" :: PackResourceFilePath
explosionAtkDescPaths = NE.fromList $ map packPath
    [ "grenade-launcher-mine-explosion-a.atk"
    , "grenade-launcher-mine-explosion-b.atk"
    , "grenade-launcher-mine-explosion-c.atk"
    ] :: NE.NonEmpty PackResourceFilePath

mineLandSoundPath = "event:/SFX Events/Player/Guns/mine-land" :: FilePath

debugHitboxColor = Color 0 155 30 155 :: Color

data MineStatus
    = MineOffStatus
    | MineOnStatus Secs

data MineData = MineData
    { _status                :: MineStatus
    , _mineOffImage          :: Image
    , _mineOnImage           :: Image
    , _mineOverlayImage      :: Image
    , _mineExplosionAtkDescs :: NE.NonEmpty AttackDescription
    , _config                :: GrenadeLauncherConfig
    }

mkMineData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Player -> m MineData
mkMineData _ =
    MineData <$>
    pure MineOffStatus <*>
    loadPackImage mineOffImagePath <*>
    loadPackImage mineOnImagePath <*>
    loadPackImage overlayImagePath <*>
    traverse loadPackAttackDescription explosionAtkDescPaths <*>
    readConfig _playerGun _grenadeLauncher

mkMine :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Player -> m (Some Projectile)
mkMine player = do
    mineData <- mkMineData player
    msgId    <- newId

    let
        playerId         = _msgId (player :: Player)
        playerPos        = _pos (player :: Player)
        cfg              = _config (mineData :: MineData)
        throwOffset      = Pos2 0.0 (-60.0)
        mineOuterWidth   = _mineOuterWidth cfg
        mineOuterHeight  = _mineOuterHeight cfg
        mineCenterOffset = Pos2 (-mineOuterWidth / 2.0) 0.0
        pos              = playerPos `vecAdd` throwOffset
        pos'             = pos `vecAdd` mineCenterOffset
        hbx              = rectHitbox pos' mineOuterWidth mineOuterHeight

    return . Some $ (mkProjectile mineData msgId hbx maxSecs)
        { _vel                  = _mineVel cfg
        , _ownerId              = playerId
        , _registeredCollisions = S.fromList
            [ ProjRegisteredSurfaceCollision
            , ProjRegisteredEnemyCollision
            ]
        , _update               = updateMine
        , _draw                 = drawMine
        , _processCollisions    = processMineCollisions
        }

updateMine :: Monad m => ProjectileUpdate MineData m
updateMine mine = return . updateMinePosVel $ mine {P._data = mineData {_status = status} :: MineData}
    where
        mineData = P._data mine
        status   = case _status (mineData :: MineData) of
            MineOffStatus          -> MineOffStatus
            MineOnStatus armingTtl -> MineOnStatus $ max 0.0 (armingTtl - timeStep)

updateMinePosVel :: Projectile MineData -> Projectile MineData
updateMinePosVel mine = mine
    { _hitbox = const hitbox'
    , _vel    = toVel2 velVec'
    }
    where
        hitbox  = projectileHitbox mine
        posVec  = toVec2 $ hitboxTopLeft hitbox
        velVec  = toVec2 $ P._vel mine
        posVec' = posVec `vecAdd` (velVec `vecMul` timeStep)
        hitbox' = setHitboxTopLeft (toPos2 posVec') hitbox
        cfg     = _config (P._data mine :: MineData)
        velVec' = velVec `vecAdd` (Vec2 0.0 (_mineGravity cfg * timeStep))

mineSurfaceHitbox :: Projectile MineData -> Hitbox
mineSurfaceHitbox mine = rectHitbox pos width height
    where
        Pos2 x y = hitboxBotCenter $ projectileHitbox mine
        cfg      = _config (P._data mine :: MineData)
        width    = _mineInnerWidth cfg
        height   = _mineOuterHeight cfg
        pos      = Pos2 (x - width / 2.0) (y - height)

mineTriggerHitbox :: Projectile MineData -> Hitbox
mineTriggerHitbox mine = rectHitbox pos width height
    where
        Pos2 x y = hitboxBotCenter $ projectileHitbox mine
        cfg      = _config (P._data mine :: MineData)
        width    = _mineOuterWidth cfg
        height   = _mineInnerHeight cfg
        pos      = Pos2 (x - width / 2.0) (y - height)

processMineCollisions :: ProjectileProcessCollisions MineData
processMineCollisions collisions mine = foldr processCollision [] collisions
    where
        processCollision :: ProjectileCollision -> [Msg ThinkCollisionMsgsPhase] -> [Msg ThinkCollisionMsgsPhase]
        processCollision collision msgs = case collision of
            ProjSurfaceCollision surfaceHbx _
                | surfaceHbx `intersectsHitbox` mineSurfaceHitbox mine -> mineSurfaceCollision surfaceHbx mine ++ msgs

            ProjEnemyCollision en
                | enemyHitbox en `intersectsHitbox` mineTriggerHitbox mine && isArmed ->
                    mineEnemyCollision mine ++ msgs

            _ -> msgs

            where
                isArmed = case _status (P._data mine :: MineData) of
                    MineOnStatus armingTtl -> armingTtl <= 0.0
                    MineOffStatus          -> False

mineSurfaceCollision :: Hitbox -> Projectile MineData -> [Msg ThinkCollisionMsgsPhase]
mineSurfaceCollision surfaceHbx mine =
    [ mkMsgTo (ProjectileMsgSetVelocity zeroVel2) mineId
    , mkMsgTo (ProjectileMsgSetHitbox hbx') mineId
    , mkMsgTo (ProjectileMsgUpdate updateStatus) mineId
    ] ++ audioMsgs
    where
        mineId = P._msgId mine
        hbx    = projectileHitbox mine
        hbx'   = if
            | hitboxTop surfaceHbx < hitboxBot hbx && hitboxTop hbx <= hitboxTop surfaceHbx ->
                let pos = Pos2 (hitboxLeft hbx) (hitboxTop surfaceHbx - hitboxHeight hbx)
                in setHitboxTopLeft pos hbx
            | otherwise                                                                     -> hbx

        audioMsgs = case _status (P._data mine :: MineData) of
            MineOffStatus  -> [mkMsg $ AudioMsgPlaySound mineLandSoundPath (hitboxCenter hbx')]
            MineOnStatus _ -> []

        updateStatus = \mn ->
            let
                mnData     = P._data mn
                armingSecs = _mineArmingSecs $ _config (mnData :: MineData)
            in mn
                { P._data = mnData
                    { _status = case _status (mnData :: MineData) of
                        MineOffStatus -> MineOnStatus armingSecs
                        status        -> status
                    } :: MineData
                }

mineEnemyCollision :: Projectile MineData -> [Msg ThinkCollisionMsgsPhase]
mineEnemyCollision mine =
    [ mkMsgTo (ProjectileMsgSetTtl 0.0) mineId
    , mkMsg $ NewUpdateProjectileMsgAddM mkExplosion
    ]
        where
            mineId           = P._msgId mine
            mineCenterPos    = hitboxCenter $ projectileHitbox mine
            mineData         = P._data mine
            mkExplosion      = do
                explosionAtkDesc <- randomChoice $ _mineExplosionAtkDescs mineData
                mkPlayerAttackProjectile mineCenterPos RightDir explosionAtkDesc

drawMine :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => ProjectileDraw MineData m
drawMine mine =
    let
        hbx      = projectileHitbox mine
        pos      = hitboxBotCenter hbx
        mineData = P._data mine
        img      = case _status (mineData :: MineData) of
            MineOffStatus  -> _mineOffImage mineData
            MineOnStatus _ -> _mineOnImage mineData
    in do
        drawImage pos RightDir worldProjectileZIndex img

        case _status (mineData :: MineData) of
            MineOffStatus  -> return ()
            MineOnStatus _ -> drawImage pos RightDir worldProjectileZIndex (_mineOverlayImage mineData)

        whenM (readSettingsConfig _debug _drawEntityHitboxes) $
            drawHitbox debugHitboxColor debugHitboxZIndex hbx

