module Player.Gun.All.SpikeGun.Sprites
    ( SpikeGunSprites(..)
    , mkSpikeGunSprites
    ) where

import Control.Monad.IO.Class (MonadIO)

import FileCache
import Window.Graphics

data SpikeGunSprites = SpikeGunSprites
    { _spike         :: Sprite
    , _spikeAlt      :: Sprite
    , _spikesPortal  :: Sprite
    , _spikesSummon1 :: Sprite
    , _spikesSummon2 :: Sprite
    , _spikesSummon3 :: Sprite
    , _spikesSummon4 :: Sprite
    , _spikesSummon5 :: Sprite
    , _spikesRing    :: Sprite
    }

mkSpikeGunSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m SpikeGunSprites
mkSpikeGunSprites =
    SpikeGunSprites <$>
    loadPackSpr "spike.spr" <*>
    loadPackSpr "spike-alt.spr" <*>
    loadPackSpr "spikes-portal.spr" <*>
    loadPackSpr "spikes-summon1-.spr" <*>
    loadPackSpr "spikes-summon2-.spr" <*>
    loadPackSpr "spikes-summon3-.spr" <*>
    loadPackSpr "spikes-summon4-.spr" <*>
    loadPackSpr "spikes-summon5-.spr" <*>
    loadPackSpr "spikes-ring.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/player/player-guns.pack" f
