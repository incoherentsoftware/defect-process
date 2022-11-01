module Level.Room.Tutorial.SandbagAir.Sprites
    ( SandbagAirSprites(..)
    , mkSandbagAirSprites
    ) where

import Control.Monad.IO.Class (MonadIO)

import Enemy.Util
import FileCache
import Window.Graphics

data SandbagAirSprites = SandbagAirSprites
    { _spawn         :: Sprite
    , _death         :: Sprite
    , _idle          :: Sprite
    , _hurt          :: Sprite
    , _fallHurt      :: Sprite
    , _fall          :: Sprite
    , _fallen        :: Sprite
    , _fallenHurt    :: Sprite
    , _knockDownFall :: Sprite
    , _launchUp      :: Sprite
    , _launched      :: Sprite
    , _launchedHurt  :: Sprite
    , _wallSplat     :: Sprite
    , _wallHurt      :: Sprite
    , _dematerialize :: Sprite
    , _rematerialize :: Sprite
    }

mkSandbagAirSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m SandbagAirSprites
mkSandbagAirSprites =
    SandbagAirSprites <$>
    loadPackSprite enemySpawnDummyBodyPath <*>
    loadPackSprite enemyDeathEffectPath <*>
    loadPackSpr "dummy-air-idle.spr" <*>
    loadPackSpr "dummy-air-hurt.spr" <*>
    loadPackSpr "dummy-air-knock-down-fall.spr" <*>
    loadPackSpr "dummy-air-fall.spr" <*>
    loadPackSpr "dummy-air-fallen.spr" <*>
    loadPackSpr "dummy-air-fallen-hurt.spr" <*>
    loadPackSpr "dummy-air-knock-down-fall.spr" <*>
    loadPackSpr "dummy-air-launch-up.spr" <*>
    loadPackSpr "dummy-air-launched.spr" <*>
    loadPackSpr "dummy-air-launched-hurt.spr" <*>
    loadPackSpr "dummy-air-wall-splat.spr" <*>
    loadPackSpr "dummy-air-wall-hurt.spr" <*>
    loadPackSpr "dummy-air-dematerialize.spr" <*>
    loadPackSpr "dummy-air-rematerialize.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/levels/level-tutorial.pack" f
