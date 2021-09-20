module Player.Overlay.All.GroundFriction
    ( mkGroundFrictionOverlay
    ) where

import Control.Monad.IO.Class (MonadIO)

import FileCache
import Player.Overlay.All.GroundFriction.Types
import Player.Overlay.Types
import Player.Types
import Player.Util
import Util
import Window.Graphics
import World.ZIndex

mkGroundFrictionOverlayData :: (FileCache m, GraphicsRead m, MonadIO m) => m GroundFrictionOverlayData
mkGroundFrictionOverlayData =
    GroundFrictionOverlayData <$>
    loadPackSpr "ground-friction.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/particles/particles.pack" f

mkGroundFrictionOverlay :: (FileCache m, GraphicsRead m, MonadIO m) => m (PlayerOverlay GroundFrictionOverlayData)
mkGroundFrictionOverlay = do
    frictionOverlayData <- mkGroundFrictionOverlayData

    return $ PlayerOverlay
        { _data   = frictionOverlayData
        , _done   = False
        , _update = update
        , _draw   = draw
        }

update :: PlayerOverlayUpdate GroundFrictionOverlayData
update _ frictionOverlay
    | spriteFinished spr = frictionOverlay {_done = True}
    | otherwise          = frictionOverlay
        { _data = frictionOverlayData {_sprite = updateSprite spr} :: GroundFrictionOverlayData
        }
    where
        frictionOverlayData = _data frictionOverlay
        spr                 = _sprite (frictionOverlayData :: GroundFrictionOverlayData)

draw :: (GraphicsReadWrite m, MonadIO m) => PlayerOverlayDraw GroundFrictionOverlayData m
draw player frictionOverlay = do
    pos    <- (_pos player `vecAdd`) <$> playerLerpOffset player
    let spr = _sprite (_data frictionOverlay :: GroundFrictionOverlayData)
    drawSprite pos (_dir player) playerOverBodyZIndex spr
