module Enemy.All.Bat.Sprites
    ( EnemySprites(..)
    , mkEnemySprites
    ) where

import Control.Monad.IO.Class (MonadIO)

import Enemy.Util
import FileCache
import Window.Graphics

data EnemySprites = EnemySprites
    { _death           :: Sprite
    , _fall            :: Sprite
    , _fallen          :: Sprite
    , _fallenHurt      :: Sprite
    , _forwardsFly     :: Sprite
    , _getUp           :: Sprite
    , _hurt            :: Sprite
    , _idle            :: Sprite
    , _knockDownFallen :: Sprite
    , _launched        :: Sprite
    , _launchedHurt    :: Sprite
    , _launchUp        :: Sprite
    , _spawn           :: Sprite
    , _upwardsFly      :: Sprite
    , _wallHurt        :: Sprite
    , _wallSplat       :: Sprite
    }

mkEnemySprites :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemySprites
mkEnemySprites =
    EnemySprites <$>
    loadPackSprite enemyDeathEffectPath <*>
    loadPackSpr "fall.spr" <*>
    loadPackSpr "fallen.spr" <*>
    loadPackSpr "fallen-hurt.spr" <*>
    loadPackSpr "forwards-fly.spr" <*>
    loadPackSpr "get-up.spr" <*>
    loadPackSpr "hurt.spr" <*>
    loadPackSpr "idle.spr" <*>
    loadPackSpr "knock-down-fall.spr" <*>
    loadPackSpr "launched.spr" <*>
    loadPackSpr "launched-hurt.spr" <*>
    loadPackSpr "launch-up.spr" <*>
    loadPackSprite enemySpawnDummyBodyPath <*>
    loadPackSpr "upwards-fly.spr" <*>
    loadPackSpr "wall-hurt.spr" <*>
    loadPackSpr "wall-splat.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/enemies/bat-enemy.pack" f
