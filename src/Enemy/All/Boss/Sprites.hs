module Enemy.All.Boss.Sprites
    ( EnemySprites(..)
    , mkEnemySprites
    ) where

import Control.Monad.IO.Class (MonadIO)

import FileCache
import Window.Graphics

data EnemySprites = EnemySprites
    { _spawn          :: Sprite
    , _death          :: Sprite
    , _airDeath       :: Sprite
    , _airDeathLand   :: Sprite
    , _idle           :: Sprite
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
    , _guard          :: Sprite
    , _airGuard       :: Sprite
    , _airGuardLand   :: Sprite
    }

mkEnemySprites :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemySprites
mkEnemySprites =
    EnemySprites <$>
    loadPackSprite (PackResourceFilePath "data/enemies/boss-enemy-spawn.pack" "spawn.spr") <*>
    loadDeathPackSpr "death.spr" <*>
    loadDeathPackSpr "air-death.spr" <*>
    loadDeathPackSpr "air-death-land.spr" <*>
    loadPackSpr "idle.spr" <*>
    loadPackSpr "hurt.spr" <*>
    loadPackSpr "air-hurt.spr" <*>
    loadPackSpr "fall.spr" <*>
    loadPackSpr "kneeling-impact.spr" <*>
    loadPackSpr "kneeling-hurt.spr" <*>
    loadPackSpr "get-up.spr" <*>
    loadPackSpr "launched.spr" <*>
    loadPackSpr "launch-up.spr" <*>
    loadPackSpr "wall-splat.spr" <*>
    loadPackSpr "wall-hurt.spr" <*>
    loadPackSpr "guard.spr" <*>
    loadPackSpr "air-guard.spr" <*>
    loadPackSpr "air-guard-land.spr"
    where
        loadDeathPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/enemies/boss-enemy-death.pack" f
        loadPackSpr      = \f -> loadPackSprite $ PackResourceFilePath "data/enemies/boss-enemy.pack" f
