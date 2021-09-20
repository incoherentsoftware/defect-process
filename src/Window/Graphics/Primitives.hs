module Window.Graphics.Primitives
    ( drawRect
    , drawLine
    , drawLines
    , drawPolygon
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.StateVar          (($=))
import Data.Traversable       (for)
import qualified Data.Vector.Storable as V
import qualified SDL

import Util
import Window.Graphics.Camera
import Window.Graphics.Color
import Window.Graphics.DrawCall
import Window.Graphics.Renderer
import Window.Graphics.Types
import Window.Graphics.Util

drawRect :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> Float -> Float -> Color -> ZIndex -> m ()
drawRect pos width height color zIndex = do
    sdlRenderer <- (_sdlRenderer . _renderer) <$> getGraphics
    Pos2 x' y'  <- cameraTransformPos pos

    let
        point = SDL.P $ SDL.V2 (round x') (round y')
        size  = SDL.V2 (round width) (round height)
        rect  = Just $ SDL.Rectangle point size

    addGraphicsDrawCall zIndex $ do
        SDL.rendererDrawColor sdlRenderer $= colorToV4 color
        SDL.fillRect sdlRenderer rect

drawLine :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> Pos2 -> Color -> ZIndex -> m ()
drawLine pos1 pos2 color zIndex = do
    sdlRenderer  <- (_sdlRenderer . _renderer) <$> getGraphics
    Pos2 x1' y1' <- cameraTransformPos pos1
    Pos2 x2' y2' <- cameraTransformPos pos2

    let
        point1 = SDL.P $ SDL.V2 (round x1') (round y1')
        point2 = SDL.P $ SDL.V2 (round x2') (round y2')

    addGraphicsDrawCall zIndex $ do
        SDL.rendererDrawColor sdlRenderer $= colorToV4 color
        SDL.drawLine sdlRenderer point1 point2

drawLines :: (GraphicsReadWrite m, MonadIO m) => [Pos2] -> Color -> ZIndex -> m ()
drawLines points color zIndex = do
    sdlRenderer <- (_sdlRenderer . _renderer) <$> getGraphics
    points'     <- for points $ \pos -> do
        Pos2 x' y' <- cameraTransformPos pos
        return . SDL.P $ SDL.V2 (round x') (round y')

    addGraphicsDrawCall zIndex $ do
        SDL.rendererDrawColor sdlRenderer $= colorToV4 color
        SDL.drawLines sdlRenderer (V.fromList points')

drawPolygon :: (GraphicsReadWrite m, MonadIO m) => [Pos2] -> Color -> ZIndex -> m ()
drawPolygon vertices color zIndex = drawLines vertices' color zIndex
    where vertices' = vertices ++ take 1 vertices
