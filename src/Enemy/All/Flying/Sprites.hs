module Enemy.All.Flying.Sprites
    ( EnemySprites(..)
    , mkEnemySprites
    ) where

import Control.Monad.IO.Class (MonadIO)

import Enemy.Util
import FileCache
import Window.Graphics

data EnemySprites = EnemySprites
    { _idle          :: Sprite
    , _hurt          :: Sprite
    , _fallHurt      :: Sprite
    , _fall          :: Sprite
    , _fallen        :: Sprite
    , _fallenHurt    :: Sprite
    , _forwardsFly   :: Sprite
    , _upwardsFly    :: Sprite
    , _getUp         :: Sprite
    , _knockDownFall :: Sprite
    , _launchUp      :: Sprite
    , _launched      :: Sprite
    , _launchedHurt  :: Sprite
    , _spawn         :: Sprite
    , _death         :: Sprite
    , _wallSplat     :: Sprite
    , _wallHurt      :: Sprite
    }

mkEnemySprites :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemySprites
mkEnemySprites =
    EnemySprites <$>
    loadPackSpr "idle.spr" <*>
    loadPackSpr "hurt.spr" <*>
    loadPackSpr "knock-down-fall.spr" <*>
    loadPackSpr "fall.spr" <*>
    loadPackSpr "fallen.spr" <*>
    loadPackSpr "fallen-hurt.spr" <*>
    loadPackSpr "forwards-fly.spr" <*>
    loadPackSpr "upwards-fly.spr" <*>
    loadPackSpr "get-up.spr" <*>
    loadPackSpr "knock-down-fall.spr" <*>
    loadPackSpr "launch-up.spr" <*>
    loadPackSpr "launched.spr" <*>
    loadPackSpr "launched-hurt.spr" <*>
    loadPackSprite enemySpawnDummyBodyPath <*>
    loadPackSprite enemyDeathEffectPath <*>
    loadPackSpr "wall-splat.spr" <*>
    loadPackSpr "wall-hurt.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/enemies/flying-enemy.pack" f
