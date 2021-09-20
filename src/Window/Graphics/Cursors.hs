module Window.Graphics.Cursors
    ( module Window.Graphics.Cursors.Types
    , mkGraphicsCursors
    , setGraphicsCursor
    , freeGraphicsCursors
    ) where


import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.StateVar          (($=))
import Foreign.C.Types        (CInt)
import qualified SDL
import qualified SDL.Image

import Configs.All.Settings
import Configs.All.Settings.Menu
import Configs.All.Settings.UI
import Util
import Window.Graphics.Cursors.Types
import Window.Graphics.Types

crosshairCursorPath = "data/cursors/crosshair-cursor.png" :: FilePath
menuCursorPath      = "data/cursors/arrow-cursor.png"     :: FilePath

mkGraphicsCursors :: MonadIO m => SettingsConfig -> m GraphicsCursors
mkGraphicsCursors settingsCfg =
    let
        hotspotPoint :: Pos2 -> SDL.Point SDL.V2 CInt
        hotspotPoint (Pos2 x y) = SDL.P $ SDL.V2 x' y'
            where
                x' = fromIntegral $ round x
                y' = fromIntegral $ round y

        crosshairHotspot = hotspotPoint $ _crosshairCursorHotspotPos (_ui settingsCfg)
        menuHotspot      = hotspotPoint $ _menuCursorHotspotPos (_menu (settingsCfg :: SettingsConfig))
    in do
        crosshairSurface <- SDL.Image.load =<< translateResourcePath crosshairCursorPath
        crosshairCursor  <- SDL.createColorCursor crosshairSurface crosshairHotspot
        SDL.freeSurface crosshairSurface

        menuSurface <- SDL.Image.load =<< translateResourcePath menuCursorPath
        menuCursor  <- SDL.createColorCursor menuSurface menuHotspot
        SDL.freeSurface menuSurface

        return $ GraphicsCursors
            { _crosshair = crosshairCursor
            , _menu      = menuCursor
            }

setGraphicsCursor :: (GraphicsReadWrite m, MonadIO m) => (GraphicsCursors -> SDL.Cursor) -> m ()
setGraphicsCursor cursorFn = do
    graphicsCursor <- _cursors <$> getGraphics
    SDL.activeCursor $= cursorFn graphicsCursor

freeGraphicsCursors :: MonadIO m => GraphicsCursors -> m ()
freeGraphicsCursors cursors = liftIO $ do
    SDL.freeCursor $ _menu (cursors :: GraphicsCursors)
    SDL.freeCursor $ _crosshair cursors
