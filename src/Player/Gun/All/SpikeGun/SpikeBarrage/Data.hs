module Player.Gun.All.SpikeGun.SpikeBarrage.Data
    ( SpikeBarrageData(..)
    , mkSpikeBarrageData
    ) where

import Attack
import Configs.All.PlayerGun.SpikeGun
import Player
import Player.Gun.All.SpikeGun.AttackDescriptions
import Player.Gun.All.SpikeGun.Data
import Util
import Window.Graphics

data SpikeBarrageData = SpikeBarrageData
    { _targetPos    :: Pos2
    , _dir          :: Direction
    , _numSpikes    :: Int
    , _releaseTtl   :: Secs
    , _spikeSpr     :: Sprite
    , _overlaySpr   :: Sprite
    , _spikeAtkDesc :: AttackDescription
    , _sprites      :: SpikeGunSprites
    , _config       :: SpikeGunConfig
    }

mkSpikeBarrageData :: SpikeGunData -> Player -> SpikeBarrageData
mkSpikeBarrageData spikeGunData player = SpikeBarrageData
    { _targetPos    = playerAimPos player
    , _dir          = _dir (player :: Player)
    , _numSpikes    = numSpikes
    , _releaseTtl   = _barrageReleaseAliveSecs cfg
    , _spikeSpr     = _sprite (spikeBarrageAtkDesc :: AttackDescription)
    , _overlaySpr   = _spikesPortal sprs
    , _spikeAtkDesc = spikeBarrageAtkDesc
    , _sprites      = sprs
    , _config       = cfg
    }
    where
        cfg                 = _config (spikeGunData :: SpikeGunData)
        spikeBarrageAtkDesc = _spikeBarrage $ _attackDescs spikeGunData
        numSpikes           = _numSpikes (spikeGunData :: SpikeGunData)
        sprs                = _sprites (spikeGunData :: SpikeGunData)
