module Player.Overlay.All
    ( PlayerOverlays(..)
    , mkPlayerOverlays
    ) where

import Control.Monad.IO.Class (MonadIO)

import FileCache
import Player.Overlay.All.GrindSparks
import Player.Overlay.All.GroundFriction
import Player.Overlay.Types
import Window.Graphics

mkPlayerOverlays :: (FileCache m, GraphicsRead m, MonadIO m) => m PlayerOverlays
mkPlayerOverlays =
    PlayerOverlays <$>
    mkGrindSparksOverlay <*>
    mkGroundFrictionOverlay
