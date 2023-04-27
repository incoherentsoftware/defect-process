module Player.Weapon.All.SpiritBlade.SpiritFormProjectile
    ( mkSpiritFormProjectile
    , setSpiritFormProjectileTriggered
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import Collision
import Id
import Msg
import Player.Util
import Player.Weapon.All.SpiritBlade.Data
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

data SpiritFormProjData = SpiritFormProjData
    { _isTriggered            :: Bool
    , _attack                 :: Attack
    , _nextAttackDescriptions :: [AttackDescription]
    }

mkSpiritFormProjectile
    :: MonadIO m
    => Pos2
    -> Direction
    -> (SpiritBladeAttackDescriptions -> AttackDescription)
    -> [SpiritBladeAttackDescriptions -> AttackDescription]
    -> SpiritBladeAttackDescriptions
    -> m (Some Projectile)
mkSpiritFormProjectile pos dir atkAppearF atksF spiritBladeAtkDescs = do
    appearAtk <- mkAttack pos dir (atkAppearF spiritBladeAtkDescs)
    let
        spiritFormData = SpiritFormProjData
            { _isTriggered            = False
            , _attack                 = appearAtk
            , _nextAttackDescriptions = map ($ spiritBladeAtkDescs) atksF
            }

    msgId  <- newId
    let hbx = dummyHitbox pos

    return . Some $ (mkProjectile spiritFormData msgId hbx maxSecs)
        { _think  = thinkSpiritFormProjectile
        , _update = updateSpiritFormProjectile
        , _draw   = drawSpiritFormProjectile
        }

thinkSpiritFormProjectile :: Monad m => ProjectileThink SpiritFormProjData m
thinkSpiritFormProjectile spiritFormProj = return $ if
    | isNextAtk ->
        [ mkMsg $ NewUpdateProjectileMsgAddsM mkNextAtkProjs
        , mkMsgTo (ProjectileMsgSetTtl 0.0) (P._msgId spiritFormProj)
        ]
    | otherwise -> []
    where
        spiritFormData = P._data spiritFormProj
        atk            = _attack spiritFormData
        isNextAtk      = _isTriggered spiritFormData && _done atk
        pos            = _pos (atk :: Attack)
        dir            = _dir (atk :: Attack)
        nextAtkDescs   = _nextAttackDescriptions spiritFormData
        mkNextAtkProjs = traverse (mkPlayerAttackProjectile pos dir) nextAtkDescs

updateSpiritFormProjectile :: Monad m => ProjectileUpdate SpiritFormProjData m
updateSpiritFormProjectile spiritFormProj = return $ spiritFormProj
    { _data = spiritFormData {_attack = updateAttack pos dir atk}
    }
    where
        spiritFormData = P._data spiritFormProj
        atk            = _attack spiritFormData
        pos            = _pos (atk :: Attack)
        dir            = _dir (atk :: Attack)

setSpiritFormProjectileTriggered :: Projectile SpiritFormProjData -> Projectile SpiritFormProjData
setSpiritFormProjectileTriggered spiritFormProj = spiritFormProj
    { P._data = (P._data spiritFormProj) {_isTriggered = True}
    }

drawSpiritFormProjectile :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw SpiritFormProjData m
drawSpiritFormProjectile spiritFormProj =
    let
        atk = _attack (P._data spiritFormProj :: SpiritFormProjData)
        vel = attackVelToVel2 (attackVel atk) zeroVel2
        pos = _pos (atk :: Attack)
        dir = _dir (atk :: Attack)
    in do
        pos' <- graphicsLerpPos pos vel
        drawSprite pos' dir worldProjectileZIndex (attackSprite atk)
