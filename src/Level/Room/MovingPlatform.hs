module Level.Room.MovingPlatform
    ( module Level.Room.MovingPlatform.Types
    , MovingPlatformJSON
    , MovingPlatform(..)
    , mkMovingPlatform
    , thinkMovingPlatform
    , updateMovingPlatform
    , drawMovingPlatform
    , movingPlatformSurface
    ) where

import Control.Monad.IO.Class (MonadIO)

import Collision.Hitbox
import Configs
import Configs.All.Settings
import Configs.All.Settings.Debug
import Constants
import FileCache
import Level.Room.MovingPlatform.JSON
import Level.Room.MovingPlatform.Types
import Msg
import Util
import Window.Graphics
import World.Surface
import World.ZIndex

debugHbxColor = Color 87 0 127 200 :: Color

mkMovingPlatform :: (FileCache m, GraphicsRead m, MonadIO m) => MovingPlatformJSON -> m MovingPlatform
mkMovingPlatform json =
    let
        leftBounds = _leftBounds (json :: MovingPlatformJSON)
        posY       = _posY (json :: MovingPlatformJSON)
    in do
        spr <- loadPackSprite $ PackResourceFilePath "data/levels/level-items.pack" "moving-platform.spr"

        return $ MovingPlatform
            { _pos         = Pos2 leftBounds posY
            , _dir         = RightDir
            , _width       = _width (json :: MovingPlatformJSON)
            , _height      = _height (json :: MovingPlatformJSON)
            , _leftBounds  = _leftBounds (json :: MovingPlatformJSON)
            , _rightBounds = _rightBounds (json :: MovingPlatformJSON)
            , _speed       = _speed (json :: MovingPlatformJSON)
            , _sprite      = spr
            }

movingPlatformTranslateDirPos :: MovingPlatform -> (Direction, Pos2)
movingPlatformTranslateDirPos movingPlat = (dir, pos `vecAdd` offset)
    where
        dir = case _dir movingPlat of
            LeftDir
                | x <= leftBounds          -> RightDir
            RightDir
                | x + width >= rightBounds -> LeftDir
            d                              -> d

        pos@(Pos2 x _) = _pos (movingPlat :: MovingPlatform)
        width          = _width (movingPlat :: MovingPlatform)
        leftBounds     = _leftBounds (movingPlat :: MovingPlatform)
        rightBounds    = _rightBounds (movingPlat :: MovingPlatform)
        speed          = _speed (movingPlat :: MovingPlatform)
        vel            = Vel2 (speed * directionNeg dir) 0.0
        offset         = toPos2 (vel `vecMul` timeStep)

thinkMovingPlatform :: MovingPlatform -> [Msg ThinkLevelMsgsPhase]
thinkMovingPlatform movingPlat = [mkMsg $ CollisionMsgMovingPlatform hbx projectedHbx]
    where
        hbx                 = movingPlatformHitbox movingPlat
        projectedPos        = snd $ movingPlatformTranslateDirPos movingPlat
        projectedMovingPlat = movingPlat {_pos = projectedPos} :: MovingPlatform
        projectedHbx        = movingPlatformHitbox projectedMovingPlat

updateMovingPlatform :: MovingPlatform -> MovingPlatform
updateMovingPlatform movingPlat = movingPlat
    { _pos    = pos
    , _dir    = dir
    , _sprite = updateSprite $ _sprite movingPlat
    }
    where (dir, pos) = movingPlatformTranslateDirPos movingPlat

movingPlatformHitbox :: MovingPlatform -> Hitbox
movingPlatformHitbox movingPlat = rectHitbox pos width height
    where
        pos    = _pos movingPlat
        width  = _width (movingPlat :: MovingPlatform)
        height = _height (movingPlat :: MovingPlatform)

-- assumes moving platform width is the moving platform sprite's width
drawMovingPlatform :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => MovingPlatform -> m ()
drawMovingPlatform movingPlat =
    let
        hbx = movingPlatformHitbox movingPlat
        pos = Pos2 (hitboxLeft hbx) (hitboxBot hbx)
        spr = _sprite movingPlat
    in do
        drawSprite pos RightDir levelItemZIndex spr

        whenM (readSettingsConfig _debug _drawItemHitboxes) $
            drawHitbox debugHbxColor levelItemZIndex hbx

movingPlatformSurface :: MovingPlatform -> Surface
movingPlatformSurface = mkPlatformSurface . movingPlatformHitbox
