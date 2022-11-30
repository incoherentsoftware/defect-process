module Enemy.All.Blob.Sprites
    ( EnemySprites(..)
    , mkEnemySprites
    ) where

import Control.Monad.IO.Class (MonadIO)

import Enemy.Util
import FileCache
import Window.Graphics

data EnemySprites = EnemySprites
    { _airHurt         :: Sprite
    , _death           :: Sprite
    , _fall            :: Sprite
    , _fallen          :: Sprite
    , _fallenHurt      :: Sprite
    , _fallenImpact    :: Sprite
    , _getUp           :: Sprite
    , _hurt            :: Sprite
    , _idle            :: Sprite
    , _knockDownFallen :: Sprite
    , _launchUp        :: Sprite
    , _launched        :: Sprite
    , _spawn           :: Sprite
    , _wallHurt        :: Sprite
    , _wallSplat       :: Sprite
    }

mkEnemySprites :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemySprites
mkEnemySprites =
    EnemySprites <$>
    loadPackSpr "air-hurt.spr" <*>
    loadPackSprite enemyDeathEffectPath <*>
    loadPackSpr "fall.spr" <*>
    loadPackSpr "fallen.spr" <*>
    loadPackSpr "fallen-hurt.spr" <*>
    loadPackSpr "fallen-impact.spr" <*>
    loadPackSpr "get-up.spr" <*>
    loadPackSpr "hurt.spr" <*>
    loadPackSpr "idle.spr" <*>
    loadPackSpr "knock-down-fallen.spr" <*>
    loadPackSpr "launch-up.spr" <*>
    loadPackSpr "launched.spr" <*>
    loadPackSprite enemySpawnDummyBodyPath <*>
    loadPackSpr "wall-hurt.spr" <*>
    loadPackSpr "wall-splat.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/enemies/blob-enemy.pack" f
