module Player.Weapon.All.SpiritBlade.CheckWallOffsetDummyProj
    ( mkCheckWallOffsetDummyProj
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Set as S

import Attack
import Collision
import Configs
import Configs.All.Settings
import Configs.All.Settings.Debug
import Id
import Msg
import Player.Weapon.All.SpiritBlade.Data
import Player.Weapon.Types as W
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

dummyProjWidth        = 424.0  :: Float
dummyProjHeight       = 100.0  :: Float
dummyProjOffsetY      = -25.0  :: OffsetY
noCollisionMaxOffsetX = 1000.0 :: OffsetX

data DummyProjData = DummyProjData
    { _dir :: Direction
    }

mkCheckWallOffsetDummyProj :: MonadIO m => Attack -> m (Some Projectile)
mkCheckWallOffsetDummyProj atk = do
    msgId <- newId
    let
        Pos2 x y = _pos atk
        dir      = _dir (atk :: Attack)
        x'       = case dir of
            LeftDir  -> x - dummyProjWidth
            RightDir -> x
        y'       = y - dummyProjHeight + dummyProjOffsetY
        pos      = Pos2 x' y'

        hbx           = rectHitbox pos dummyProjWidth dummyProjHeight
        dummyProjData = DummyProjData dir

    return . Some $ (mkProjectile dummyProjData msgId hbx maxSecs)
        { _draw                 = draw
        , _processCollisions    = processCollisions
        , _registeredCollisions = S.singleton ProjRegisteredSurfaceCollision
        }

processCollisions :: ProjectileProcessCollisions DummyProjData
processCollisions collisions dummyProj = case _dir (P._data dummyProj :: DummyProjData) of
    LeftDir  ->
        let
            surfaceRight = maybeMaximum $ map hitboxRight surfaceHbxs
            offsetX      = maybe (-noCollisionMaxOffsetX) (subtract $ hitboxRight dummyProjHbx) surfaceRight
        in
            [ mkMsg $ PlayerMsgUpdateWeapon (setSpiritBladeCheckedWallOffsetX offsetX)
            , mkMsgTo (ProjectileMsgSetTtl 0.0) (_msgId dummyProj)
            ]
    RightDir ->
        let
            surfaceLeft = maybeMinimum $ map hitboxLeft surfaceHbxs
            offsetX     = maybe noCollisionMaxOffsetX (subtract $ hitboxLeft dummyProjHbx) surfaceLeft
        in
            [ mkMsg $ PlayerMsgUpdateWeapon (setSpiritBladeCheckedWallOffsetX offsetX)
            , mkMsgTo (ProjectileMsgSetTtl 0.0) (_msgId dummyProj)
            ]
    where
        setSpiritBladeCheckedWallOffsetX :: OffsetX -> Weapon SpiritBladeData -> Weapon SpiritBladeData
        setSpiritBladeCheckedWallOffsetX wallOffsetX spiritBlade = spiritBlade
            { _data = (W._data spiritBlade) {_checkedWallOffsetX = wallOffsetX}
            }

        processCollision :: ProjectileCollision -> [Hitbox] -> [Hitbox]
        processCollision collision !hbxs = case collision of
            ProjSurfaceCollision hbx _ -> hbx:hbxs
            _                          -> hbxs

        surfaceHbxs  = foldr processCollision [] collisions
        dummyProjHbx = projectileHitbox dummyProj

draw :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => ProjectileDraw DummyProjData m
draw dummyProj = whenM (readSettingsConfig _debug _drawEntityHitboxes) $
    let debugColor = Color 0 0 155 125
    in drawHitbox debugColor debugHitboxZIndex (projectileHitbox dummyProj)
