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

speedRailSegmentWidth = 499.0 :: Float
speedRailHeight       = 15.0  :: Float

debugHbxColor = Color 255 0 220 255 :: Color

data SpeedRail = SpeedRail
    { _pos         :: Pos2
    , _dir         :: Direction
    , _numSegments :: Int
    , _sprite      :: Sprite
    }

mkSpeedRail :: (FileCache m, GraphicsRead m, MonadIO m) => SpeedRailJSON -> m SpeedRail
mkSpeedRail json = do
    spr <- loadPackSprite $ PackResourceFilePath "data/levels/level-items.pack" "speed-rail.spr"

    return $ SpeedRail
        { _pos         = _pos (json :: SpeedRailJSON)
        , _dir         = _direction (json :: SpeedRailJSON)
        , _numSegments = _numSegments (json :: SpeedRailJSON)
        , _sprite = spr
        }

speedRailHitbox :: SpeedRail -> Hitbox
speedRailHitbox speedRail = rectHitbox pos width speedRailHeight
    where
        pos         = _pos (speedRail :: SpeedRail)
        numSegments = _numSegments (speedRail :: SpeedRail)
        width       = fromIntegral numSegments * speedRailSegmentWidth

speedRailSurface :: SpeedRail -> Surface
speedRailSurface speedRail = Surface
    { _type   = SpeedRailSurface $ _dir speedRail
    , _hitbox = speedRailHitbox speedRail
    } :: Surface

updateSpeedRail :: SpeedRail -> SpeedRail
updateSpeedRail speedRail = speedRail {_sprite = updateSprite (_sprite speedRail)}

drawSpeedRail :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => SpeedRail -> m ()
drawSpeedRail speedRail =
    let
        hbx         = speedRailHitbox speedRail
        numSegments = _numSegments (speedRail :: SpeedRail)
        dir         = _dir speedRail
        spr         = _sprite speedRail
    in do
        whenM (readSettingsConfig _debug _drawItemHitboxes) $
            drawHitbox debugHbxColor levelItemZIndex hbx

        for_ [1..numSegments] $ \i ->
            let
                x                    = hitboxLeft hbx + (realToFrac i - 1) * speedRailSegmentWidth
                x'
                    | dir == LeftDir = x + speedRailSegmentWidth
                    | otherwise      = x
                pos                  = Pos2 x' (hitboxBot hbx)
            in drawSprite pos dir levelItemZIndex spr
