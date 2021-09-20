module Level.Room.Debug
    ( drawRoomSurfaceHitboxesDebug
    , drawRoomPortalHitboxesDebug
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (for_)
import Data.Functor           ((<&>))
import qualified Data.Set as S

import Collision
import Configs
import Configs.All.Settings
import Configs.All.Settings.Debug
import Level.Room.ImageLayer
import Level.Room.Types
import Level.Room.Util
import Util
import Window.Graphics
import World.Surface
import World.ZIndex

generalSurfaceDebugColor  = Color 100 100 100 255 :: Color
platformSurfaceDebugColor = Color 150 150 150 150 :: Color

portalDebugColor = Color 163 15 255 150 :: Color

isNoForegroundImageLayersDebug :: Room -> Bool
isNoForegroundImageLayersDebug room =
    and [_zIndex il `S.notMember` levelFgImageLayerZIndices | il <- _imageLayers room]

drawRoomSurfaceHitboxesDebug :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => Room -> m ()
drawRoomSurfaceHitboxesDebug room =
    let
        surfaces        = _surfaces room
        generalSurfaces = [s | s <- surfaces, _type (s :: Surface) == GeneralSurface]
        platforms       = [s | s <- surfaces, _type (s :: Surface) == PlatformSurface]
    in do
        debugGeneralSurfaceHbxs <- readSettingsConfig _debug _drawSurfaceHitboxes
        debugPlatformHbxs       <- readSettingsConfig _debug _drawPlatformHitboxes

        when (debugGeneralSurfaceHbxs || isNoForegroundImageLayersDebug room) $
            for_ generalSurfaces $ \s ->
                let hbx = _hitbox (s :: Surface)
                in drawHitbox generalSurfaceDebugColor debugHitboxZIndex hbx

        when debugPlatformHbxs $
            for_ platforms $ \s ->
                let hbx = _hitbox (s :: Surface)
                in drawHitbox platformSurfaceDebugColor debugHitboxZIndex hbx

drawRoomPortalHitboxesDebug :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => Room -> m ()
drawRoomPortalHitboxesDebug room =
    whenM (readSettingsConfig _debug _drawPortalHitboxes <&> (|| isNoForegroundImageLayersDebug room)) $
        for_ (roomPortalHitboxes room) $ \hbx -> drawHitbox portalDebugColor debugHitboxZIndex hbx
