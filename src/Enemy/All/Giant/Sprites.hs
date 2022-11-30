module Enemy.All.Giant.Sprites
    ( EnemySprites(..)
    , mkEnemySprites
    ) where

import Control.Monad.IO.Class (MonadIO)

import Enemy.Util
import FileCache
import Window.Graphics

data EnemySprites = EnemySprites
    { _idle     :: Sprite
    , _spawn    :: Sprite
    , _death    :: Sprite
    , _walk     :: Sprite
    , _backWalk :: Sprite
    }

mkEnemySprites :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemySprites
mkEnemySprites =
    EnemySprites <$>
    loadPackSpr "idle.spr" <*>
    loadPackSprite enemySpawnDummyBodyPath <*>
    loadPackSprite enemyDeathEffectPath <*>
    loadPackSpr "walk.spr" <*>
    loadPackSpr "back-walk.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/enemies/giant-enemy.pack" f
