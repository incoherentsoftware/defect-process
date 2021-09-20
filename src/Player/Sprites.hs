module Player.Sprites
    ( PlayerSprites(..)
    , mkPlayerSprites
    ) where

import Control.Monad.IO.Class (MonadIO)

import FileCache
import Window.Graphics

data PlayerSprites = PlayerSprites
    { _idle       :: Sprite
    , _walk       :: Sprite
    , _hurt       :: Sprite
    , _jump       :: Sprite
    , _fall       :: Sprite
    , _doubleJump :: Sprite
    , _grind      :: Sprite
    , _spawn      :: Sprite
    , _death      :: Sprite
    , _warpOut    :: Sprite
    }

mkPlayerSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m PlayerSprites
mkPlayerSprites =
    PlayerSprites <$>
    loadPackSpr "idle.spr" <*>
    loadPackSpr "walk.spr" <*>
    loadPackSpr "hurt.spr" <*>
    loadPackSpr "jump.spr" <*>
    loadPackSpr "fall.spr" <*>
    loadPackSpr "double-jump.spr" <*>
    loadPackSpr "grind.spr" <*>
    loadPackSpr "spawn.spr" <*>
    loadPackSpr "death.spr" <*>
    loadPackSpr "warp-out.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/player/player-movement.pack" f
