module Player.Gun.All.SpikeGun.SpikeBarrage
    ( mkSpikeBarrage
    ) where

import Control.Monad.IO.Class (MonadIO)

import Collision
import Configs
import Configs.All.PlayerGun.SpikeGun
import Constants
import Id
import Msg
import Player
import Player.Gun.All.SpikeGun.Data
import Player.Gun.All.SpikeGun.Spike
import Player.Gun.All.SpikeGun.SpikeBarrage.Data
import Player.Gun.All.SpikeGun.Util
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

spikeReleaseSoundPath = "event:/SFX Events/Player/Guns/spike-release" :: FilePath

mkSpikeBarrage :: (ConfigsRead m, MonadIO m) => SpikeGunData -> Player -> m (Some Projectile)
mkSpikeBarrage spikeGunData player =
    let
        spikeBarrageData = mkSpikeBarrageData spikeGunData player
        dummyHbx         = dummyHitbox $ calculateSpikeBarragePos spikeGunData player
    in do
        msgId <- newId
        return . Some $ (mkProjectile spikeBarrageData msgId dummyHbx maxSecs)
            { _think  = thinkSpikeBarrage
            , _update = updateSpikeBarrage
            , _draw   = drawSpikeBarrage
            }

thinkSpikeBarrage :: Monad m => ProjectileThink SpikeBarrageData m
thinkSpikeBarrage spikeBarrage = return $ if
    | releaseTtl <= 0.0 ->
        [ mkMsg $ NewUpdateProjectileMsgAddM (mkSpike spikeBarrage)
        , mkMsg $ ParticleMsgAddM (mkSpikeGunBarrageSpikeAppearParticle spikeBarrage)
        , mkMsg $ AudioMsgPlaySound spikeReleaseSoundPath pos
        ]
    | otherwise         -> []
    where
        pos              = hitboxTopLeft $ projectileHitbox spikeBarrage
        spikeBarrageData = _data spikeBarrage
        releaseTtl       = _releaseTtl spikeBarrageData

updateSpikeBarrage :: Monad m => ProjectileUpdate SpikeBarrageData m
updateSpikeBarrage spikeBarrage = return $ if
    | numSpikes <= 0 -> spikeBarrage {_ttl = 0.0}

    | releaseTtl <= 0.0 -> spikeBarrage
        { _data = spikeBarrageData
            { _numSpikes  = numSpikes - 1
            , _releaseTtl = _barrageCooldown $ _config (spikeBarrageData :: SpikeBarrageData)
            , _overlaySpr = updateSprite $ _overlaySpr spikeBarrageData
            }
        }

    | otherwise -> spikeBarrage
        { _data = spikeBarrageData
            { _releaseTtl = releaseTtl - timeStep
            , _overlaySpr = updateSprite $ _overlaySpr spikeBarrageData
            }
        }

    where
        spikeBarrageData = _data spikeBarrage
        numSpikes        = _numSpikes (spikeBarrageData :: SpikeBarrageData)
        releaseTtl       = _releaseTtl spikeBarrageData

drawSpikeBarrage :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => ProjectileDraw SpikeBarrageData m
drawSpikeBarrage spikeBarrage = do
    playerCfg <- _player <$> readConfigs
    let
        spikePos         = calculateSpikeBarrageSpikePos spikeBarrage playerCfg
        spikeBarrageData = P._data spikeBarrage
        angle            = calculateSpikeBarrageSpikeAngle spikePos (_targetPos spikeBarrageData)
    drawSpriteRotated spikePos RightDir playerGunOverlayZIndex angle (_overlaySpr spikeBarrageData)
