module Player.Gun.All.SpikeGun.Data
    ( module Player.Gun.All.SpikeGun.Sprites
    , SpikeGunData(..)
    , mkSpikeGunData
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.PlayerGun
import Configs.All.PlayerGun.SpikeGun
import FileCache
import Id
import Msg
import Player.Gun.All.SpikeGun.AttackDescriptions
import Player.Gun.All.SpikeGun.Sprites
import Window.Graphics

data SpikeGunData = SpikeGunData
    { _numSpikes     :: Int
    , _summonSoundId :: MsgId
    , _summonSpr     :: Maybe Sprite
    , _sprites       :: SpikeGunSprites
    , _attackDescs   :: SpikeGunAttackDescriptions
    , _config        :: SpikeGunConfig
    }

mkSpikeGunData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m SpikeGunData
mkSpikeGunData = do
    soundId     <- newId
    sprs        <- mkSpikeGunSprites
    attackDescs <- mkSpikeGunAttackDescs
    cfg         <- readConfig _playerGun _spikeGun

    return $ SpikeGunData
        { _numSpikes     = 0
        , _summonSoundId = soundId
        , _summonSpr     = Nothing
        , _sprites       = sprs
        , _attackDescs   = attackDescs
        , _config        = cfg
        }
