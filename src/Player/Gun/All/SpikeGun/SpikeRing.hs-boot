module Player.Gun.All.SpikeGun.SpikeRing
    ( SpikeRing(..)
    , mkSpikeRing
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map as M

import Configs.All.PlayerGun.SpikeGun
import Id
import Msg
import Player.Gun.All.SpikeGun.Sprites
import Util

data SpikeRing = SpikeRing
    { _ringAngle                :: Radians
    , _ringSpikeAltAngleOffsets :: M.Map MsgId Radians
    , _soundHashedId            :: HashedId
    , _sprites                  :: SpikeGunSprites
    , _config                   :: SpikeGunConfig
    }

mkSpikeRing :: MonadIO m => SpikeGunSprites -> SpikeGunConfig -> m SpikeRing
