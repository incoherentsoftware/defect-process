module Player.Overlay.All.GrindSparks
    ( mkGrindSparksOverlay
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.Level
import FileCache
import Player.Flags
import Player.Overlay.All.GrindSparks.Types
import Player.Overlay.Types
import Player.Util
import Util
import Window.Graphics
import World.ZIndex
import {-# SOURCE #-} Player.Types

mkGrindSparksOverlayData :: (FileCache m, GraphicsRead m, MonadIO m) => m GrindSparksOverlayData
mkGrindSparksOverlayData =
    GrindSparksOverlayData <$>
    loadPackSpr "grind-sparks.spr" <*>
    loadPackSpr "grind-sparks-slow.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/particles/particles.pack" f

mkGrindSparksOverlay :: (FileCache m, GraphicsRead m, MonadIO m) => m (PlayerOverlay GrindSparksOverlayData)
mkGrindSparksOverlay = do
    sparksOverlayData <- mkGrindSparksOverlayData

    return $ PlayerOverlay
        { _data   = sparksOverlayData
        , _done   = False
        , _update = update
        , _draw   = draw
        }

update :: PlayerOverlayUpdate GrindSparksOverlayData
update player sparksOverlay
    | not (_onSpeedRail (_flags player)) = sparksOverlay {_done = True}
    | otherwise                          =
        let
            sparksOverlayData  = _data sparksOverlay
            sparksOverlayData' = sparksOverlayData
                { _sprite     = updateSprite $ _sprite (sparksOverlayData :: GrindSparksOverlayData)
                , _slowSprite = updateSprite $ _slowSprite (sparksOverlayData :: GrindSparksOverlayData)
                } :: GrindSparksOverlayData
        in sparksOverlay {_data = sparksOverlayData'}

draw :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => PlayerOverlayDraw GrindSparksOverlayData m
draw player sparksOverlay = do
    slowSpeedThreshold <- readConfig _level _speedRailSlowSpeedThreshold

    let
        velX                                = vecX $ _vel player
        sparksOverlayData                   = _data sparksOverlay
        spr
            | abs velX < slowSpeedThreshold = _slowSprite sparksOverlayData
            | otherwise                     = _sprite (sparksOverlayData :: GrindSparksOverlayData)

    pos <- (_pos player `vecAdd`) <$> playerLerpOffset player
    drawSprite pos (_dir player) playerOverBodyZIndex spr
