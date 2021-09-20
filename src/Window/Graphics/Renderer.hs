module Window.Graphics.Renderer
    ( module Window.Graphics.Renderer.Types
    , mkRenderer
    , freeRenderer
    , beginRenderer
    , endRenderer
    , rendererDestRect
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef             (newIORef, readIORef)
import Data.StateVar          (($=))
import Foreign.C.Types        (CInt)
import qualified SDL

import Configs.All.Settings.Render
import Id
import Window.Graphics.Renderer.Types
import Window.Graphics.Texture.Types

sdTargetTextureWidth   = 1920 :: Int
sdTargetTextureHeight  = 1080 :: Int
sdTargetTextureRawSize = 2048 :: CInt

hdTargetTextureWidth   = 3840 :: Int
hdTargetTextureHeight  = 2160 :: Int
hdTargetTextureRawSize = 4096 :: CInt

mkTargetTexture :: MonadIO m => SDL.Renderer -> RenderMode -> m Texture
mkTargetTexture sdlRenderer renderMode =
    Texture <$>
    pure NullId <*>
    SDL.createTexture sdlRenderer SDL.RGBA8888 SDL.TextureAccessTarget size <*>
    pure width <*>
    pure height
    where
        rawWidthHeight = case renderMode of
            RenderSD -> sdTargetTextureRawSize
            RenderHD -> hdTargetTextureRawSize
        size           = SDL.V2 rawWidthHeight rawWidthHeight

        (width, height) = case renderMode of
            RenderSD -> (sdTargetTextureWidth, sdTargetTextureHeight)
            RenderHD -> (hdTargetTextureWidth, hdTargetTextureHeight)

mkRenderer :: MonadIO m => SDL.Window -> Int -> Int -> RenderConfig -> m Renderer
mkRenderer window winWidth winHeight renderCfg =
    let
        sdlRendererConfig = SDL.RendererConfig
            { SDL.rendererType          = if
                | _vsync renderCfg -> SDL.AcceleratedVSyncRenderer
                | otherwise        -> SDL.AcceleratedRenderer
            , SDL.rendererTargetTexture = True
            }

        renderMode
            | winWidth > sdTargetTextureWidth && winHeight > sdTargetTextureHeight = RenderHD
            | otherwise                                                            = RenderSD
    in do
        when (_hintOpenGL renderCfg) $
            SDL.HintRenderDriver $= SDL.OpenGL
        SDL.HintRenderScaleQuality $= SDL.ScaleLinear

        sdlRenderer      <- SDL.createRenderer window (-1) sdlRendererConfig
        targetTexture    <- mkTargetTexture sdlRenderer renderMode
        targetTextureRef <- liftIO $ newIORef targetTexture

        SDL.rendererDrawBlendMode sdlRenderer $= SDL.BlendAlphaBlend
        case renderMode of
            RenderSD -> return ()
            RenderHD -> SDL.rendererScale sdlRenderer $= SDL.V2 2.0 2.0

        return $ Renderer
            { _sdlRenderer      = sdlRenderer
            , _targetTextureRef = targetTextureRef
            , _renderMode       = renderMode
            }

freeRenderer :: MonadIO m => Renderer -> m ()
freeRenderer renderer = SDL.destroyRenderer $ _sdlRenderer renderer

clearSdlRenderer :: MonadIO m => SDL.Renderer -> m ()
clearSdlRenderer sdlRenderer = do
    SDL.rendererDrawColor sdlRenderer $= SDL.V4 0 0 0 255
    SDL.clear sdlRenderer

beginRenderer :: MonadIO m => Renderer -> m ()
beginRenderer renderer = do
    let targetTextureRef = _targetTextureRef renderer
    targetTexture       <- liftIO $ readIORef targetTextureRef
    let
        targetSdlTexture = _sdlTexture targetTexture
        sdlRenderer      = _sdlRenderer renderer

    SDL.rendererRenderTarget sdlRenderer $= Just targetSdlTexture
    clearSdlRenderer sdlRenderer

endRenderer :: MonadIO m => Int -> Int -> Renderer -> m ()
endRenderer winWidth winHeight renderer = do
    targetTexture  <- liftIO $ readIORef (_targetTextureRef renderer)
    let sdlRenderer = _sdlRenderer renderer

    SDL.rendererRenderTarget sdlRenderer $= Nothing
    clearSdlRenderer sdlRenderer

    let
        srcPoint         = SDL.P $ SDL.V2 0 0
        renderWidth      = _width (targetTexture :: Texture)
        renderHeight     = _height (targetTexture :: Texture)
        srcSize          = SDL.V2 (fromIntegral renderWidth) (fromIntegral renderHeight)
        srcRect          = Just $ SDL.Rectangle srcPoint srcSize
        targetSdlTexture = _sdlTexture targetTexture

    destRect <- Just <$> rendererDestRect winWidth winHeight renderer
    SDL.copyEx sdlRenderer targetSdlTexture srcRect destRect 0 Nothing (SDL.V2 False False)
    SDL.present sdlRenderer

rendererDestRect :: MonadIO m => Int -> Int -> Renderer -> m (SDL.Rectangle CInt)
rendererDestRect winWidth winHeight renderer = do
    targetTexture <- liftIO $ readIORef (_targetTextureRef renderer)

    let
        renderWidth   = _width (targetTexture :: Texture)
        renderHeight  = _height (targetTexture :: Texture)
        winWidthCInt  = fromIntegral winWidth
        winHeightCInt = fromIntegral winHeight

        (destPoint, destSize) =
            let
                h = round $ fromIntegral renderHeight / fromIntegral renderWidth * fromIntegral winWidth
                y = (winHeightCInt - h) `quot` 2
                w = round $ fromIntegral renderWidth / fromIntegral renderHeight * fromIntegral winHeight
                x = (winWidthCInt - w) `quot` 2
            in if
                | h <= winHeightCInt ->
                    ( SDL.P $ SDL.V2 0 y
                    , SDL.V2 winWidthCInt h
                    )
                | otherwise          ->
                    ( SDL.P $ SDL.V2 x 0
                    , SDL.V2 w winHeightCInt
                    )

    return $ SDL.Rectangle destPoint destSize
