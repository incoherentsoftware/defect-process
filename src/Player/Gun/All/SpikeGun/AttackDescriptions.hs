module Player.Gun.All.SpikeGun.AttackDescriptions
    ( SpikeGunAttackDescriptions(..)
    , mkSpikeGunAttackDescs
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack.Description
import FileCache
import Window.Graphics

data SpikeGunAttackDescriptions = SpikeGunAttackDescriptions
    { _spikeBarrage :: AttackDescription
    }

mkSpikeGunAttackDescs :: (FileCache m, GraphicsRead m, MonadIO m) => m SpikeGunAttackDescriptions
mkSpikeGunAttackDescs =
    SpikeGunAttackDescriptions <$>
    loadPackAttackDescription (PackResourceFilePath "data/player/player-guns.pack" "spike-barrage.atk")
