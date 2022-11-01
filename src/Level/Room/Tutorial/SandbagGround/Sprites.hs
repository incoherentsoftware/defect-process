module Level.Room.Tutorial.SandbagGround.Sprites
    ( SandbagGroundSprites(..)
    , mkSandbagGroundSprites
    ) where

import Control.Monad.IO.Class (MonadIO)

import Enemy.Util
import FileCache
import Window.Graphics

data SandbagGroundSprites = SandbagGroundSprites
    { _spawn           :: Sprite
    , _death           :: Sprite
    , _idle            :: Sprite
    , _hurt            :: Sprite
    , _airHurt         :: Sprite
    , _fall            :: Sprite
    , _launchUp        :: Sprite
    , _launched        :: Sprite
    , _fallen          :: Sprite
    , _fallenHurt      :: Sprite
    , _knockDownFallen :: Sprite
    , _dematerialize   :: Sprite
    , _rematerialize   :: Sprite
    , _wallSplat       :: Sprite
    , _wallHurt        :: Sprite
    }

mkSandbagGroundSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m SandbagGroundSprites
mkSandbagGroundSprites =
    SandbagGroundSprites <$>
    loadPackSprite enemySpawnDummyBodyPath <*>
    loadPackSprite enemyDeathEffectPath <*>
    loadPackSpr "dummy-ground-idle.spr" <*>
    loadPackSpr "dummy-ground-hurt.spr" <*>
    loadPackSpr "dummy-ground-air-hurt.spr" <*>
    loadPackSpr "dummy-ground-fall.spr" <*>
    loadPackSpr "dummy-ground-launch-up.spr" <*>
    loadPackSpr "dummy-ground-launched.spr" <*>
    loadPackSpr "dummy-ground-fallen.spr" <*>
    loadPackSpr "dummy-ground-fallen-hurt.spr" <*>
    loadPackSpr "dummy-ground-knock-down-fallen.spr" <*>
    loadPackSpr "dummy-ground-dematerialize.spr" <*>
    loadPackSpr "dummy-ground-rematerialize.spr" <*>
    loadPackSpr "dummy-ground-wall-splat.spr" <*>
    loadPackSpr "dummy-ground-wall-hurt.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/levels/level-tutorial.pack" f
