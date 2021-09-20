module Level.Room.DisappearingPlatform
    ( DisappearingPlatform(..)
    , mkDisappearingPlatform
    , updateDisappearingPlatform
    , drawDisappearingPlatform
    , disappearingPlatformSurface
    ) where

import Control.Monad.IO.Class (MonadIO)

import Collision.Hitbox
import Configs
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import FileCache
import Level.Room.DisappearingPlatform.JSON
import Util
import Window.Graphics
import World.Surface
import World.ZIndex

levelItemsPack    = PackResourceFilePath "data/levels/level-items.pack"    :: FilePath -> PackResourceFilePath
appearIdleSprPath = levelItemsPack "disappearing-platform-appear-idle.spr" :: PackResourceFilePath
disappearSprPath  = levelItemsPack "disappearing-platform-disappear.spr"   :: PackResourceFilePath

debugHitboxColor = Color 38 127 0 200 :: Color

data DisappearingPlatformSprites = DisappearingPlatformSprites
    { _appearIdle :: Sprite
    , _disappear  :: Sprite
    }

mkDisappearingPlatformSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m DisappearingPlatformSprites
mkDisappearingPlatformSprites =
    DisappearingPlatformSprites <$>
    loadPackSprite appearIdleSprPath <*>
    loadPackSprite disappearSprPath

data DisappearingPlatform = DisappearingPlatform
    { _positions :: [(Secs, Hitbox)]
    , _sprite    :: Sprite
    , _sprites   :: DisappearingPlatformSprites
    }

mkDisappearingPlatform
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => DisappearingPlatformJSON
    -> m DisappearingPlatform
mkDisappearingPlatform jsons = do
    sprs <- mkDisappearingPlatformSprites

    return $ DisappearingPlatform
        { _positions = cycle
            [ (_durationSecs json, hbx)
            | json <- jsons
            , let hbx = _fromJSON $ _hitbox (json :: DisappearingPlatformEntryJSON)
            ]
        , _sprite    = _appearIdle sprs
        , _sprites   = sprs
        }

isDisappearSpriteActive :: DisappearingPlatform -> Bool
isDisappearSpriteActive disappearPlat = spr == _disappear sprs && not (spriteFinished spr)
    where
        spr  = _sprite disappearPlat
        sprs = _sprites disappearPlat

updateDisappearingPlatform :: DisappearingPlatform -> DisappearingPlatform
updateDisappearingPlatform disappearPlat = case _positions disappearPlat of
    []              -> disappearPlat
    ((ttl, hbx):ps) ->
        let
            ttl'          = ttl - timeStep
            spr           = _sprite disappearPlat
            sprs          = _sprites disappearPlat
            appearIdleSpr = _appearIdle sprs
            disappearSpr  = _disappear sprs
        in if
            | ttl' <= 0.0 -> if
                | spr == appearIdleSpr                      -> disappearPlat {_sprite = disappearSpr}
                | spr == disappearSpr && spriteFinished spr -> disappearPlat
                    { _positions = ps
                    , _sprite    = appearIdleSpr
                    }
                | otherwise                                 -> disappearPlat {_sprite = updateSprite spr}
            | otherwise   -> disappearPlat
                { _positions = (ttl', hbx):ps
                , _sprite    = updateSprite spr
                }

disappearingPlatformHitbox :: DisappearingPlatform -> Hitbox
disappearingPlatformHitbox disappearPlat = case _positions disappearPlat of
    []                                          -> dummyHitbox zeroPos2
    ((_, hbx):_)
        | isDisappearSpriteActive disappearPlat -> dummyHitbox $ hitboxBotLeft hbx
        | otherwise                             -> hbx

drawDisappearingPlatform :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => DisappearingPlatform -> m ()
drawDisappearingPlatform disappearPlat =
    let
        hbx = disappearingPlatformHitbox disappearPlat
        pos = hitboxBotLeft hbx
    in do
        drawSprite pos RightDir levelItemZIndex (_sprite disappearPlat)

        whenM (readSettingsConfig _debug _drawItemHitboxes) $
            drawHitbox debugHitboxColor levelItemZIndex hbx

disappearingPlatformSurface :: DisappearingPlatform -> Surface
disappearingPlatformSurface = mkPlatformSurface . disappearingPlatformHitbox
