module Enemy.All.Zombie.Sprites
    ( EnemySprites(..)
    , mkEnemySprites
    ) where

import Control.Monad.IO.Class (MonadIO)

import Enemy.Util
import FileCache
import Window.Graphics

data EnemySprites = EnemySprites
    { _spawn           :: Sprite
    , _idle            :: Sprite
    , _hurt            :: Sprite
    , _airHurt         :: Sprite
    , _walk            :: Sprite
    , _death           :: Sprite
    , _fall            :: Sprite
    , _launchUp        :: Sprite
    , _launched        :: Sprite
    , _fallen          :: Sprite
    , _fallenHurt      :: Sprite
    , _knockDownFallen :: Sprite
    , _getUp           :: Sprite
    , _wallSplat       :: Sprite
    , _wallHurt        :: Sprite
    }

mkEnemySprites :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemySprites
mkEnemySprites =
    EnemySprites <$>
    loadPackSprite enemySpawnDummyBodyPath <*>
    loadPackSpr "idle.spr" <*>
    loadPackSpr "hurt.spr" <*>
    loadPackSpr "air-hurt.spr" <*>
    loadPackSpr "walk.spr" <*>
    loadPackSprite enemyDeathEffectPath <*>
    loadPackSpr "fall.spr" <*>
    loadPackSpr "launch-up.spr" <*>
    loadPackSpr "launched.spr" <*>
    loadPackSpr "fallen.spr" <*>
    loadPackSpr "fallen-hurt.spr" <*>
    loadPackSpr "knock-down-fallen.spr" <*>
    loadPackSpr "get-up.spr" <*>
    loadPackSpr "wall-splat.spr" <*>
    loadPackSpr "wall-hurt.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/enemies/zombie-enemy.pack" f
