module Enemy.All.Bomb.Sprites
    ( EnemySprites(..)
    , mkEnemySprites
    , isExplodeSprite
    ) where

import Control.Monad.IO.Class (MonadIO)

import Enemy.Util
import FileCache
import Window.Graphics

data EnemySprites = EnemySprites
    { _airHurt         :: Sprite
    , _fall            :: Sprite
    , _explode         :: Sprite
    , _explodeLaunched :: Sprite
    , _explodeFallen   :: Sprite
    , _explodeWall     :: Sprite
    , _hurt            :: Sprite
    , _idle            :: Sprite
    , _run             :: Sprite
    , _launchUp        :: Sprite
    , _launched        :: Sprite
    , _spawn           :: Sprite
    }

mkEnemySprites :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemySprites
mkEnemySprites =
    EnemySprites <$>
    loadPackSpr "air-hurt.spr" <*>
    loadPackSpr "fall.spr" <*>
    loadPackSpr "explode.spr" <*>
    loadPackSpr "explode-launched.spr" <*>
    loadPackSpr "explode-fallen.spr" <*>
    loadPackSpr "explode-wall.spr" <*>
    loadPackSpr "hurt.spr" <*>
    loadPackSpr "idle.spr" <*>
    loadPackSpr "run.spr" <*>
    loadPackSpr "launch-up.spr" <*>
    loadPackSpr "launched.spr" <*>
    loadPackSprite enemySpawnDummyBodyPath
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/enemies/bomb-enemy.pack" f

isExplodeSprite :: Sprite -> EnemySprites -> Bool
isExplodeSprite spr sprs =
    spr == _explode sprs || spr == _explodeLaunched sprs || spr == _explodeFallen sprs || spr == _explodeWall sprs
