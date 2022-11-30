module Player.Gun.All.Shotgun.BurnFlame
    ( mkBurnFlame
    ) where

import Control.Monad          (unless)
import Control.Monad.IO.Class (MonadIO)

import Attack
import Attack.Hit
import Collision
import Configs.All.PlayerGun.Shotgun
import Constants
import FileCache
import Id
import Msg
import Particle.All.Simple
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

packPath                    = \f -> PackResourceFilePath "data/player/player-guns.pack" f
burnOverlayDisappearSprPath = packPath "shotgun-burn-overlay-disappear.spr"  :: PackResourceFilePath
burnFlameSoundFilePath      = "event:/SFX Events/Player/Guns/shotgun-burn-c" :: FilePath

damageFrameTagName = FrameTagName "damage" :: FrameTagName
delaySecs          = 0.016                 :: Secs

data BurnFlameData = BurnFlameData
    { _delayTtl      :: Secs
    , _burnTtl       :: Secs
    , _enemyMsgId    :: MsgId
    , _attack        :: Attack
    , _soundHashedId :: HashedId
    }

mkBurnFlameData :: (CollisionEntity e, FileCache m, GraphicsRead m, MonadIO m) => e -> m BurnFlameData
mkBurnFlameData enemy = do
    let pos        = hitboxCenter $ collisionEntityHitbox enemy
    atk           <- mkAttack pos RightDir =<< loadPackAttackDescription (packPath "shotgun-burn-overlay.atk")
    soundHashedId <- hashId <$> newId

    return $ BurnFlameData
        { _delayTtl      = delaySecs
        , _burnTtl       = maxSecs
        , _enemyMsgId    = collisionEntityMsgId enemy
        , _attack        = atk
        , _soundHashedId = soundHashedId
        }

mkBurnFlame
    :: (CollisionEntity e, FileCache m, GraphicsRead m, MonadIO m)
    => Projectile ShotgunConfig
    -> e
    -> m (Some Projectile)
mkBurnFlame shot enemy = do
    burnFlameData <- mkBurnFlameData enemy
    msgId         <- newId
    let dummyHbx   = dummyHitbox $ hitboxCenter (collisionEntityHitbox enemy)

    return . Some $ (mkProjectile burnFlameData msgId dummyHbx maxSecs)
        { _ownerId = _ownerId shot
        , _think   = thinkBurnFlame
        , _update  = updateBurnFlame
        , _draw    = drawBurnFlame
        }

thinkBurnFlame :: MsgsRead ThinkProjectileMsgsPhase m => ProjectileThink BurnFlameData m
thinkBurnFlame burnFlame = (burnDoneMsgs ++) . (damageMsgs ++) . (audioMsgs ++) . processMsg <$> readMsgs
    where
        burnFlameId   = _msgId burnFlame
        burnFlamePos  = hitboxTopLeft $ projectileHitbox burnFlame
        burnFlameData = _data burnFlame
        burnFlameAtk  = _attack burnFlameData

        processMsg :: [InfoMsgPayload] -> [Msg ThinkProjectileMsgsPhase]
        processMsg []     = []
        processMsg (d:ds) = case d of
            InfoMsgProjectilePos projPos projOwnerId projId
                -- remove duplicate/older burn flames on same enemy
                | projOwnerId == burnFlameOwnerId && posApproxEq projPos burnFlamePos &&
                  hashId projId > hashId burnFlameId -> [mkMsgTo (ProjectileMsgSetTtl 0.0) burnFlameId]
            _                                        -> processMsg ds
            where
                posApproxEq      = \(Pos2 x1 y1) (Pos2 x2 y2) -> approxEq x1 x2 && approxEq y1 y2
                burnFlameOwnerId = _ownerId burnFlame

        audioMsgs
            | _delayTtl burnFlameData > 0.0 = []
            | otherwise                     =
                let soundHashedId = _soundHashedId (burnFlameData :: BurnFlameData)
                in [mkMsg $ AudioMsgPlaySoundContinuous burnFlameSoundFilePath soundHashedId burnFlamePos]

        damageMsgs
            | damageFrameTagName `isAttackFrameTag` burnFlameAtk && attackFrameChanged burnFlameAtk =
                let
                    atkHit          = mkAttackHit burnFlameAtk
                    enemyId         = _enemyMsgId burnFlameData
                    atkHitSoundMsgs = attackHitSoundMessages burnFlamePos burnFlameAtk
                in mkMsgTo (HurtMsgAttackHit atkHit) enemyId:atkHitSoundMsgs
            | otherwise                                                                             = []

        burnDoneMsgs
            | _burnTtl burnFlameData > 0.0 = []
            | otherwise                    =
                let
                    mkBurnFlameDisappearParticle =
                        loadSimpleParticle burnFlamePos RightDir worldProjectileZIndex burnOverlayDisappearSprPath
                in
                    [ mkMsgTo (ProjectileMsgSetTtl 0.0) burnFlameId
                    , mkMsg $ ParticleMsgAddM mkBurnFlameDisappearParticle
                    ]

updateBurnFlame :: MsgsRead UpdateProjectileMsgsPhase m => ProjectileUpdate BurnFlameData m
updateBurnFlame burnFlame
    | delayTtl > 0.0 = return $ burnFlame {_data = burnFlameData {_delayTtl = delayTtl - timeStep}}
    | otherwise      = update burnFlame <$> readMsgs
    where
        burnFlameData = P._data burnFlame
        delayTtl      = _delayTtl burnFlameData

        update :: Projectile BurnFlameData -> [InfoMsgPayload] -> Projectile BurnFlameData
        update burnFlameProj []     = burnFlameProj {_data = burnFlameData {_burnTtl = 0.0}}
        update burnFlameProj (d:ds) = case d of
            InfoMsgEnemyPos enHbx enId
                | enId == _enemyMsgId burnFlameData && not (isDummyHitbox enHbx) ->
                    let
                        burnFlameHbx  = dummyHitbox $ hitboxCenter enHbx
                        burnFlameAtk  = _attack (burnFlameData :: BurnFlameData)
                        burnFlameAtk' = updateAttack (hitboxCenter burnFlameHbx) RightDir burnFlameAtk
                    in burnFlameProj
                        { _data   = burnFlameData
                            { _burnTtl = if _done burnFlameAtk' then 0.0 else _burnTtl burnFlameData
                            , _attack  = burnFlameAtk'
                            }
                        , _hitbox = const burnFlameHbx
                        }

            _ -> update burnFlameProj ds

drawBurnFlame :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw BurnFlameData m
drawBurnFlame burnFlame = unless (_delayTtl burnFlameData > 0) $
    drawSprite pos RightDir worldProjectileZIndex spr
    where
        pos           = hitboxCenter $ projectileHitbox burnFlame
        burnFlameData = P._data burnFlame :: BurnFlameData
        spr           = attackSprite $ _attack (burnFlameData :: BurnFlameData)
