module Level.Room.SpeedRail
    ( SpeedRail(..)
    , mkSpeedRail
    , speedRailSurface
    , updateSpeedRail
    , drawSpeedRail
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (for_)

import Configs
import Configs.All.Settings
import Configs.All.Settings.Debug
import Collision.Hitbox
import FileCache
import Level.Room.SpeedRail.JSON
import Util
import Window.Graphics
import World.Surface
import World.ZIndex

debugHbxColor = Color 255 0 220 255 :: Color

data SpeedRail = SpeedRail
    { _dir    :: Direction
    , _hitbox :: Hitbox
    , _sprite :: Sprite
    }

mkSpeedRail :: (FileCache m, GraphicsRead m, MonadIO m) => SpeedRailJSON -> m SpeedRail
mkSpeedRail json = do
    spr <- loadPackSprite $ PackResourceFilePath "data/levels/level-items.pack" "speed-rail.spr"

    return $ SpeedRail
        { _dir    = _direction (json :: SpeedRailJSON)
        , _hitbox = _fromJSON $ _hitbox (json :: SpeedRailJSON)
        , _sprite = spr
        }

speedRailSurface :: SpeedRail -> Surface
speedRailSurface speedRail = Surface
    { _type   = SpeedRailSurface $ _dir speedRail
    , _hitbox = _hitbox (speedRail :: SpeedRail)
    } :: Surface

updateSpeedRail :: SpeedRail -> SpeedRail
updateSpeedRail speedRail = speedRail {_sprite = updateSprite (_sprite speedRail)}

-- assumes total speed rail width is evenly divided by the speed rail sprite width
drawSpeedRail :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => SpeedRail -> m ()
drawSpeedRail speedRail =
    let
        hbx        = _hitbox (speedRail :: SpeedRail)
        totalWidth = round $ hitboxWidth hbx
        spr        = _sprite speedRail
        sprWidthF  = spriteImageWidth spr
        sprWidth   = round sprWidthF
        dir        = _dir speedRail
    in do
        whenM (readSettingsConfig _debug _drawItemHitboxes) $
            drawHitbox debugHbxColor levelItemZIndex hbx

        for_ [1..totalWidth `div` sprWidth] $ \i ->
            let
                x                    = hitboxLeft hbx + (realToFrac i - 1) * sprWidthF
                x'
                    | dir == LeftDir = x + sprWidthF
                    | otherwise      = x
                pos                  = Pos2 x' (hitboxBot hbx)
            in drawSprite pos dir levelItemZIndex spr
