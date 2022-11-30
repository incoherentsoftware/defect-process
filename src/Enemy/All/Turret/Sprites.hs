module Enemy.All.Turret.Sprites
    ( EnemySprites(..)
    , mkEnemySprites
    ) where

import Control.Monad.IO.Class (MonadIO)

import Enemy.Util
import FileCache
import Window.Graphics

data EnemySprites = EnemySprites
    { _spawn         :: Sprite
    , _idle          :: Sprite
    , _hurt          :: Sprite
    , _death         :: Sprite
    , _attackBeamMid :: Sprite
    , _attackBeamEnd :: Sprite
    }

mkEnemySprites :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemySprites
mkEnemySprites =
    EnemySprites <$>
    loadPackSprite enemySpawnDummyBodyPath <*>
    loadPackSpr "idle.spr" <*>
    loadPackSpr "hurt.spr" <*>
    loadPackSprite enemyDeathEffectPath <*>
    loadPackSpr "attack-beam-mid.spr" <*>
    loadPackSpr "attack-beam-end.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/enemies/turret-enemy.pack" f
