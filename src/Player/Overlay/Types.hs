module Player.Overlay.Types
    ( PlayerOverlayUpdate
    , PlayerOverlayDraw
    , PlayerOverlay(..)
    , PlayerOverlays(..)
    ) where

import AppEnv
import Msg.Phase
import Player.Overlay.All.GrindSparks.Types
import Player.Overlay.All.GroundFriction.Types
import {-# SOURCE #-} Player.Types

type PlayerOverlayUpdate d = Player -> PlayerOverlay d -> PlayerOverlay d
type PlayerOverlayDraw d m = Player -> PlayerOverlay d -> m ()

data PlayerOverlay d = PlayerOverlay
    { _data   :: d
    , _done   :: Bool
    , _update :: PlayerOverlayUpdate d
    , _draw   :: PlayerOverlayDraw d (AppEnv DrawMsgsPhase)
    }

data PlayerOverlays = PlayerOverlays
    { _grindSparks    :: PlayerOverlay GrindSparksOverlayData
    , _groundFriction :: PlayerOverlay GroundFrictionOverlayData
    }
