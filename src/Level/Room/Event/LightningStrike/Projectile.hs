module Level.Room.Event.LightningStrike.Projectile
    ( mkRelativeLightningProjectile
    , mkExactLightningProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.Set as S

import Attack
import Attack.Projectile
import Collision
import FileCache
import Id
import Msg
import Projectile as P
import Util
import Window.Graphics

projOffsetY = -165.0       :: PosY
projWidth   = 1920.0 * 2.0 :: Float
projHeight  = 50.0         :: Float

lightningStrikeAtkDescPath =
    PackResourceFilePath "data/levels/level-items.pack" "lightning-pre-strike.atk" :: PackResourceFilePath

data LightningTarget
    = ExactTarget Pos2
    | RelativeTarget Float PosY

data LightningProjData = LightningProjData
    { _target  :: LightningTarget
    , _atkDesc :: AttackDescription
    }

mkLightningProjData :: (FileCache m, GraphicsRead m, MonadIO m) => LightningTarget -> m LightningProjData
mkLightningProjData target = do
    atkDesc <- loadPackAttackDescription lightningStrikeAtkDescPath
    return $ LightningProjData
        { _target  = target
        , _atkDesc = atkDesc
        }

mkRelativeLightningProjectile :: (FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> Float -> m (Some Projectile)
mkRelativeLightningProjectile (Pos2 x y) xMultiplier = do
    lightningProjData <- mkLightningProjData $ RelativeTarget xMultiplier y
    msgId             <- newId
    let
        pos = Pos2 (x - projWidth / 2.0) (y - projHeight + projOffsetY)
        hbx = rectHitbox pos projWidth projHeight

    return . Some $ (mkProjectile lightningProjData msgId hbx maxSecs)
        { _registeredCollisions = S.fromList [ProjRegisteredSurfaceCollision]
        , _update               = updateLightningProj
        , _processCollisions    = processLightningProjCollisions
        }

mkExactLightningProjectile :: (FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> m (Some Projectile)
mkExactLightningProjectile pos = do
    lightningProjData <- mkLightningProjData $ ExactTarget pos
    msgId             <- newId
    let hbx            = rectHitbox pos projWidth projHeight

    return . Some $ (mkProjectile lightningProjData msgId hbx maxSecs)
        { _update = updateLightningProj
        }

updateLightningProj :: Monad m => ProjectileUpdate LightningProjData m
updateLightningProj lightningProj = return $ lightningProj
    { _think = \p ->
        let
            pData   = P._data p
            atkDesc = _atkDesc pData
            pos     = case _target pData of
                ExactTarget pos'             -> pos'
                RelativeTarget xMultiplier y ->
                    let
                        hbx = projectileHitbox p
                        x   = hitboxLeft hbx + hitboxWidth hbx * xMultiplier
                    in Pos2 x y
        in return
            [ mkMsg $ NewUpdateProjectileMsgAddM (mkEnemyAttackProjectile pos RightDir atkDesc)
            , mkMsgTo (ProjectileMsgSetTtl 0.0) (P._msgId p)
            ]
    }

processLightningProjCollisions :: ProjectileProcessCollisions LightningProjData
processLightningProjCollisions collisions lightningProj =
    [mkMsgTo (ProjectileMsgUpdate update) (P._msgId lightningProj)]
        where
            processSurfaceCollision :: ProjectileCollision -> [Hitbox] -> [Hitbox]
            processSurfaceCollision collision !hbxs = case collision of
                ProjSurfaceCollision hbx _ -> hbx:hbxs
                _                          -> hbxs

            surfaceHbxs     = foldr processSurfaceCollision [] collisions
            projHbx         = projectileHitbox lightningProj
            innerLeftWallX  = fromMaybe (hitboxLeft projHbx) (maybeMinimum $ map hitboxRight surfaceHbxs)
            innerRightWallX = fromMaybe (hitboxRight projHbx) (maybeMaximum $ map hitboxLeft surfaceHbxs)

            update :: Projectile LightningProjData -> Projectile LightningProjData
            update p = p
                { _hitbox =
                    let
                        hbx = projectileHitbox p
                        pos = Pos2 innerLeftWallX (hitboxTop hbx)
                    in const $ rectHitbox pos (innerRightWallX - innerLeftWallX) (hitboxHeight hbx)
                }
