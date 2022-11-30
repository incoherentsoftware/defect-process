module Enemy.All.BubbleTurret.Sprites
    ( EnemySprites(..)
    , mkEnemySprites
    ) where

import Control.Monad.IO.Class (MonadIO)

import Enemy.Util
import FileCache
import Window.Graphics

data EnemySprites = EnemySprites
    { _spawn :: Sprite
    , _idle  :: Sprite
    , _hurt  :: Sprite
    , _death :: Sprite
    }

mkEnemySprites :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemySprites
mkEnemySprites =
    EnemySprites <$>
    loadPackSprite enemySpawnDummyBodyPath <*>
    loadPackSpr "idle.spr" <*>
    loadPackSpr "hurt.spr" <*>
    loadPackSprite enemyDeathEffectPath
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/enemies/bubble-turret-enemy.pack" f
