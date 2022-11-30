module Enemy.All.Claws.Sprites
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
    , _walkBack        :: Sprite
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
    , _projectileHit   :: Sprite
    , _projectileFade  :: Sprite
    }

mkEnemySprites :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemySprites
mkEnemySprites =
    EnemySprites <$>
    loadPackSprite enemySpawnDummyBodyPath <*>
    loadSpr "idle.spr" <*>
    loadSpr "hurt.spr" <*>
    loadSpr "air-hurt.spr" <*>
    loadSpr "walk.spr" <*>
    loadSpr "back-walk.spr" <*>
    loadPackSprite enemyDeathEffectPath <*>
    loadSpr "fall.spr" <*>
    loadSpr "launch-up.spr" <*>
    loadSpr "launched.spr" <*>
    loadSpr "fallen.spr" <*>
    loadSpr "fallen-hurt.spr" <*>
    loadSpr "knock-down-fallen.spr" <*>
    loadSpr "get-up.spr" <*>
    loadSpr "wall-splat.spr" <*>
    loadSpr "wall-hurt.spr" <*>
    loadSpr "attack-projectile-hit.spr" <*>
    loadSpr "attack-projectile-fade.spr"
    where loadSpr = \f -> loadPackSprite $ PackResourceFilePath "data/enemies/claws-enemy.pack" f
