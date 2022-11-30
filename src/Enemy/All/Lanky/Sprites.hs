module Enemy.All.Lanky.Sprites
    ( EnemySprites(..)
    , mkEnemySprites
    ) where

import Control.Monad.IO.Class (MonadIO)

import Enemy.Util
import FileCache
import Window.Graphics

data EnemySprites = EnemySprites
    { _spawn          :: Sprite
    , _death          :: Sprite
    , _idle           :: Sprite
    , _idleAura       :: Sprite
    , _walk           :: Sprite
    , _walkAura       :: Sprite
    , _backWalk       :: Sprite
    , _backWalkAura   :: Sprite
    , _auraBreak      :: Sprite
    , _hurt           :: Sprite
    , _airHurt        :: Sprite
    , _fall           :: Sprite
    , _kneelingImpact :: Sprite
    , _kneelingHurt   :: Sprite
    , _getUp          :: Sprite
    , _launched       :: Sprite
    , _launchUp       :: Sprite
    , _wallSplat      :: Sprite
    , _wallHurt       :: Sprite
    }

mkEnemySprites :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemySprites
mkEnemySprites =
    EnemySprites <$>
    loadPackSprite enemySpawnDummyBodyPath <*>
    loadPackSprite enemyDeathEffectPath <*>
    loadPackSpr "idle.spr" <*>
    loadPackSpr "idle-aura.spr" <*>
    loadPackSpr "walk.spr" <*>
    loadPackSpr "walk-aura.spr" <*>
    loadPackSpr "back-walk.spr" <*>
    loadPackSpr "back-walk-aura.spr" <*>
    loadPackSpr "aura-break.spr" <*>
    loadPackSpr "hurt.spr" <*>
    loadPackSpr "air-hurt.spr" <*>
    loadPackSpr "fall.spr" <*>
    loadPackSpr "kneeling-impact.spr" <*>
    loadPackSpr "kneeling-hurt.spr" <*>
    loadPackSpr "get-up.spr" <*>
    loadPackSpr "launched.spr" <*>
    loadPackSpr "launch-up.spr" <*>
    loadPackSpr "wall-splat.spr" <*>
    loadPackSpr "wall-hurt.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/enemies/lanky-enemy.pack" f
