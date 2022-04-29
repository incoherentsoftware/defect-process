module Window.Graphics
    ( module Window.Graphics.BlendMode
    , module Window.Graphics.Camera
    , module Window.Graphics.Color
    , module Window.Graphics.Cursors
    , module Window.Graphics.DisplayText
    , module Window.Graphics.Event
    , module Window.Graphics.Fonts
    , module Window.Graphics.Image
    , module Window.Graphics.Image.Parse
    , module Window.Graphics.InputDisplayText
    , module Window.Graphics.Opacity
    , module Window.Graphics.Primitives
    , module Window.Graphics.Sprite
    , module Window.Graphics.SubRect
    , module Window.Graphics.SymbolDisplayText
    , module Window.Graphics.Texture
    , module Window.Graphics.Util
    , DrawCall
    , Lerp(..)
    , Graphics(Graphics, _renderer, _window, _textureManagerVar)
    , GraphicsRead(..)
    , GraphicsReadWrite
    , addGraphicsDrawCall
    , mkGraphics
    , freeGraphicsAndExit
    , freeGraphicsAllTextures
    , displayGraphics
    , showCursor
    , setGraphicsLerp
    , graphicsLerpPos
    , graphicsLerpOffset
    , graphicsClipRect
    , setGraphicsClipRect
    , graphicsBlendMode
    , setGraphicsBlendMode
    , getGraphicsDesktopResolution
    , getGraphicsAvailableResolutions
    , getGraphicsResolution
    , setGraphicsResolution
    , getGraphicsWindowSize
    , getGraphicsDisplayCount
    , getGraphicsDisplayIndex
    , setGraphicsDisplayIndex
    , getGraphicsWindowMode
    , setGraphicsWindowMode
    , isGraphicsDisplayIndexChanged
    ) where

import Control.Concurrent          (threadDelay)
import Control.Concurrent.STM.TVar (newTVarIO, readTVarIO, writeTVar)
import Control.Monad               (when, void)
import Control.Monad.IO.Class      (MonadIO, liftIO)
import Control.Monad.STM           (atomically)
import Control.Monad.Trans.Except  (runExceptT, throwE)
import Data.Foldable               (for_)
import Data.Functor                ((<&>))
import Data.IORef                  (newIORef, readIORef, writeIORef)
import Data.StateVar               (($=))
import Data.Traversable            (for)
import Foreign.Marshal.Alloc       (alloca)
import Foreign.Ptr                 (nullPtr)
import Foreign.Storable            (peek, poke)
import System.Exit                 (exitSuccess)
import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.Text as T
import qualified SDL
import qualified SDL.Internal.Numbered as SDL.Internal
import qualified SDL.Internal.Types as SDL.Internal
import qualified SDL.Raw

import Configs
import Configs.All.Settings
import Configs.All.Settings.Render
import Constants
import Util
import Util.Time
import Window.Graphics.BlendMode
import Window.Graphics.Camera
import Window.Graphics.Color
import Window.Graphics.Cursors
import Window.Graphics.DisplayText
import Window.Graphics.DrawCall
import Window.Graphics.Event
import Window.Graphics.Fonts
import Window.Graphics.Image
import Window.Graphics.Image.Parse
import Window.Graphics.InputDisplayText
import Window.Graphics.Opacity
import Window.Graphics.Primitives
import Window.Graphics.Renderer
import Window.Graphics.Sprite
import Window.Graphics.SubRect
import Window.Graphics.SymbolDisplayText
import Window.Graphics.Texture
import Window.Graphics.Texture.Manager
import Window.Graphics.Types
import Window.Graphics.Util

minRenderSecs = 0.002 :: Secs

mkGraphics :: MonadIO m => Int -> Int -> T.Text -> SettingsConfig -> m Graphics
mkGraphics winWidth winHeight windowTitle settingsCfg =
    let
        renderCfg    = _render settingsCfg
        windowSize   = SDL.V2 (fromIntegral winWidth) (fromIntegral winHeight)
        sdlWindowCfg = SDL.defaultWindow
            { SDL.windowInitialSize = windowSize
            , SDL.windowMode        = case _winMode renderCfg of
                FullscreenMode        -> SDL.Fullscreen
                FullscreenDesktopMode -> SDL.FullscreenDesktop
                WindowedMode          -> SDL.Windowed
            , SDL.windowHighDPI     = True
            }
    in do
        window   <- SDL.createWindow windowTitle sdlWindowCfg
        renderer <- mkRenderer window winWidth winHeight renderCfg

        fonts           <- mkGraphicsFonts
        cursors         <- mkGraphicsCursors settingsCfg
        drawCallsRef    <- liftIO $ newIORef IM.empty
        cameraPosRef    <- liftIO $ newIORef zeroPos2
        cameraOffsetRef <- liftIO $ newIORef zeroPos2
        cameraSpaceRef  <- liftIO $ newIORef CameraWorldSpace
        lerpRef         <- liftIO $ newIORef 0.0
        clipRectRef     <- liftIO $ newIORef Nothing
        blendModeRef    <- liftIO $ newIORef BlendModeAlpha
        textureMgrVar   <- liftIO $ newTVarIO mkTextureManager

        return $ Graphics
            { _window            = window
            , _renderer          = renderer
            , _fonts             = fonts
            , _cursors           = cursors
            , _drawCallsRef      = drawCallsRef
            , _cameraPos         = cameraPosRef
            , _cameraOffset      = cameraOffsetRef
            , _cameraSpace       = cameraSpaceRef
            , _lerpRef           = lerpRef
            , _clipRectRef       = clipRectRef
            , _blendModeRef      = blendModeRef
            , _textureManagerVar = textureMgrVar
            }

freeGraphicsAndExit :: forall m a. (GraphicsReadWrite m, MonadIO m) => m a
freeGraphicsAndExit = do
    gfx <- getGraphics
    --freeGraphicsCursors $ _cursors gfx
    --freeGraphicsFonts $ _fonts gfx
    --freeRenderer $ _renderer gfx
    SDL.destroyWindow $ _window gfx
    liftIO exitSuccess

freeGraphicsAllTextures :: (GraphicsReadWrite m, MonadIO m) => m ()
freeGraphicsAllTextures = do
    textureMgrVar <- _textureManagerVar <$> getGraphics
    textureMgr    <- freeTextureManagerAllTextures =<< liftIO (readTVarIO textureMgrVar)
    liftIO . atomically $ writeTVar textureMgrVar textureMgr

displayGraphics :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => m ()
displayGraphics = do
    gfx        <- getGraphics
    renderTime <- mkTime

    let
        renderer                          = _renderer gfx
        SDL.Internal.Renderer rawRenderer = _sdlRenderer renderer
    beginRenderer renderer

    let drawCallsRef = _drawCallsRef gfx
    drawCalls       <- liftIO $ readIORef drawCallsRef
    let drawCalls'   = concatMap (reverse . snd) (IM.toDescList drawCalls)

    for_ drawCalls' $ \drawCall -> do
        liftIO $ case _clipRect (drawCall :: DrawCallInternal) of
            Nothing       -> void $ SDL.Raw.renderSetClipRect rawRenderer nullPtr
            Just clipRect ->
                alloca $ \rectPtr -> do
                    poke rectPtr clipRect
                    void $ SDL.Raw.renderSetClipRect rawRenderer rectPtr

        setCameraPos $ _cameraPos (drawCall :: DrawCallInternal)
        setCameraOffset $ _cameraOffset (drawCall :: DrawCallInternal)
        setCameraSpace $ _cameraSpace (drawCall :: DrawCallInternal)
        liftIO $ _draw drawCall

    liftIO $ writeIORef drawCallsRef IM.empty

    renderCfg <- readConfig _settings _render
    endRenderer (_winWidth renderCfg) (_winHeight renderCfg) renderer

    renderSecs <- _diffSecs <$> updateTime renderTime
    when (renderSecs < minRenderSecs) $
        liftIO . threadDelay . round $ (minRenderSecs - renderSecs) * 1000000

showCursor :: MonadIO m => Bool -> m ()
showCursor visible = SDL.cursorVisible $= visible

graphicsLerp :: (GraphicsRead m, MonadIO m) => m Lerp
graphicsLerp = do
    gfx <- getGraphics
    liftIO $ readIORef (_lerpRef gfx)

setGraphicsLerp :: (GraphicsReadWrite m, MonadIO m) => Lerp -> m ()
setGraphicsLerp lerp = do
    lerpRef <- _lerpRef <$> getGraphics
    liftIO $ writeIORef lerpRef lerp

graphicsLerpPos :: (GraphicsRead m, MonadIO m) => Pos2 -> Vel2 -> m Pos2
graphicsLerpPos pos vel = do
    Lerp lerp <- graphicsLerp
    let pos'   = pos `vecAdd` toPos2 (vel `vecMul` timeStep)
    return $ (pos `vecMul` (1.0 - lerp)) `vecAdd` (pos' `vecMul` lerp)

graphicsLerpOffset :: (GraphicsRead m, MonadIO m) => Vel2 -> m Pos2
graphicsLerpOffset = graphicsLerpPos zeroPos2

graphicsClipRect :: (GraphicsRead m, MonadIO m) => m (Maybe SDL.Raw.Rect)
graphicsClipRect = do
    gfx <- getGraphics
    liftIO $ readIORef (_clipRectRef gfx)

setGraphicsClipRect :: (GraphicsRead m, MonadIO m) => Maybe SDL.Raw.Rect -> m ()
setGraphicsClipRect clipRect = do
    clipRectRef <- _clipRectRef <$> getGraphics
    liftIO $ writeIORef clipRectRef clipRect

graphicsBlendMode :: (GraphicsRead m, MonadIO m) => m BlendMode
graphicsBlendMode = do
    gfx <- getGraphics
    liftIO $ readIORef (_blendModeRef gfx)

setGraphicsBlendMode :: (GraphicsRead m, MonadIO m) => BlendMode -> m ()
setGraphicsBlendMode blendMode = do
    blendModeRef <- _blendModeRef <$> getGraphics
    liftIO $ writeIORef blendModeRef blendMode

getGraphicsDesktopResolution :: (GraphicsRead m, MonadIO m) => m (Maybe (Int, Int))
getGraphicsDesktopResolution = do
    SDL.Internal.Window rawWindow <- _window <$> getGraphics
    displayIndex                  <- SDL.Raw.getWindowDisplayIndex rawWindow

    liftIO $
        alloca $ \displayModePtr ->
            SDL.Raw.getDesktopDisplayMode displayIndex displayModePtr >>= \r -> if
                | r < 0     -> return Nothing
                | otherwise -> do
                    displayMode <- peek displayModePtr
                    return . Just $
                        ( fromIntegral (SDL.Raw.displayModeW displayMode)
                        , fromIntegral (SDL.Raw.displayModeH displayMode)
                        )

getGraphicsAvailableResolutions :: (GraphicsRead m, MonadIO m) => m [(Int, Int)]
getGraphicsAvailableResolutions = do
    SDL.Internal.Window rawWindow <- _window <$> getGraphics

    resolutions <- runExceptT $ do
        displayIndex <- SDL.Raw.getWindowDisplayIndex rawWindow
        when (displayIndex < 0) $
            throwE $ "SDL.Raw.getWindowDisplayIndex: " ++ show displayIndex

        numDisplayModes <- SDL.Raw.getNumDisplayModes displayIndex
        when (numDisplayModes < 0) $
            throwE $ "SDL.Raw.getNumDisplayModes: " ++ show numDisplayModes

        let
            filterResolutions = \rs ->
                (L.nub . filter (\(w, h) -> w >= minWindowWidth && h >= minWindowHeight)) rs

        (filterResolutions <$>) $
            for [0..numDisplayModes] $ \modeIndex -> do
                displayMode <- liftIO $
                    alloca $ \displayModePtr ->
                        SDL.Raw.getDisplayMode displayIndex modeIndex displayModePtr >>= \r -> if
                            | r < 0     -> return Nothing
                            | otherwise -> Just <$> peek displayModePtr

                return $ case displayMode of
                    Nothing -> (0, 0)
                    Just dm -> (fromIntegral (SDL.Raw.displayModeW dm), fromIntegral (SDL.Raw.displayModeH dm))

    return $ either (const []) id resolutions

getGraphicsResolution :: (GraphicsRead m, MonadIO m) => m (Maybe (Int, Int))
getGraphicsResolution = do
    SDL.Internal.Renderer rawRenderer <- _sdlRenderer . _renderer <$> getGraphics
    liftIO $
        alloca $ \widthPtr ->
            alloca $ \heightPtr ->
                SDL.Raw.getRendererOutputSize rawRenderer widthPtr heightPtr >>= \r -> if
                    | r < 0     -> return Nothing
                    | otherwise -> do
                        width  <- fromIntegral <$> peek widthPtr
                        height <- fromIntegral <$> peek heightPtr
                        return $ Just (width, height)

setGraphicsResolution :: (GraphicsReadWrite m, MonadIO m) => Int -> Int -> m ()
setGraphicsResolution winWidth winHeight
    | winWidth < minWindowWidth || winHeight < minWindowHeight = return ()
    | otherwise                                                = do
        window@(SDL.Internal.Window rawWindow) <- _window <$> getGraphics

        getGraphicsWindowMode >>= \case
            FullscreenMode -> liftIO $
                alloca $ \displayModePtr ->
                    SDL.Raw.getWindowDisplayMode rawWindow displayModePtr >>= \r -> if
                        | r < 0     -> return ()
                        | otherwise -> do
                            displayMode <- peek displayModePtr <&> \dm -> dm
                                { SDL.Raw.displayModeW = fromIntegral winWidth
                                , SDL.Raw.displayModeH = fromIntegral winHeight
                                }
                            poke displayModePtr displayMode
                            void $ SDL.Raw.setWindowDisplayMode rawWindow displayModePtr

            FullscreenDesktopMode -> return ()

            WindowedMode -> do
                SDL.windowSize window $= SDL.V2 (fromIntegral winWidth) (fromIntegral winHeight)
                setGraphicsDisplayIndex =<< getGraphicsDisplayIndex  -- recenter window

getGraphicsWindowSize :: (GraphicsRead m, MonadIO m) => m (Int, Int)
getGraphicsWindowSize = do
    SDL.Internal.Window rawWindow <- _window <$> getGraphics
    liftIO $
        alloca $ \widthPtr ->
            alloca $ \heightPtr -> do
                SDL.Raw.getWindowSize rawWindow widthPtr heightPtr
                width  <- fromIntegral <$> peek widthPtr
                height <- fromIntegral <$> peek heightPtr
                return (width, height)

getGraphicsDisplayCount :: (GraphicsRead m, MonadIO m) => m Int
getGraphicsDisplayCount = fromIntegral <$> SDL.Raw.getNumVideoDisplays

getGraphicsDisplayIndex :: (GraphicsRead m, MonadIO m) => m Int
getGraphicsDisplayIndex = do
    SDL.Internal.Window rawWindow <- _window <$> getGraphics
    fromIntegral <$> SDL.Raw.getWindowDisplayIndex rawWindow

setGraphicsDisplayIndex :: (GraphicsReadWrite m, MonadIO m) => Int -> m ()
setGraphicsDisplayIndex winDisplayIndex =
    let
        sdlWindowPos = case winDisplayIndex of
            1  -> SDL.Raw.SDL_WINDOWPOS_CENTERED_DISPLAY_1
            2  -> SDL.Raw.SDL_WINDOWPOS_CENTERED_DISPLAY_2
            3  -> SDL.Raw.SDL_WINDOWPOS_CENTERED_DISPLAY_3
            4  -> SDL.Raw.SDL_WINDOWPOS_CENTERED_DISPLAY_4
            5  -> SDL.Raw.SDL_WINDOWPOS_CENTERED_DISPLAY_5
            6  -> SDL.Raw.SDL_WINDOWPOS_CENTERED_DISPLAY_6
            7  -> SDL.Raw.SDL_WINDOWPOS_CENTERED_DISPLAY_7
            8  -> SDL.Raw.SDL_WINDOWPOS_CENTERED_DISPLAY_8
            9  -> SDL.Raw.SDL_WINDOWPOS_CENTERED_DISPLAY_9
            10 -> SDL.Raw.SDL_WINDOWPOS_CENTERED_DISPLAY_10
            11 -> SDL.Raw.SDL_WINDOWPOS_CENTERED_DISPLAY_11
            12 -> SDL.Raw.SDL_WINDOWPOS_CENTERED_DISPLAY_12
            13 -> SDL.Raw.SDL_WINDOWPOS_CENTERED_DISPLAY_13
            14 -> SDL.Raw.SDL_WINDOWPOS_CENTERED_DISPLAY_14
            15 -> SDL.Raw.SDL_WINDOWPOS_CENTERED_DISPLAY_15
            _  -> SDL.Raw.SDL_WINDOWPOS_CENTERED_DISPLAY_0
    in do
        SDL.Internal.Window rawWindow <- _window <$> getGraphics
        SDL.Raw.setWindowPosition rawWindow sdlWindowPos sdlWindowPos

getGraphicsWindowMode :: (GraphicsRead m, MonadIO m) => m WindowMode
getGraphicsWindowMode = do
    SDL.Internal.Window rawWindow <- _window <$> getGraphics
    (SDL.Internal.fromNumber <$> SDL.Raw.getWindowFlags rawWindow) <&> \case
        SDL.Fullscreen        -> FullscreenMode
        SDL.FullscreenDesktop -> FullscreenDesktopMode
        _                     -> WindowedMode

setGraphicsWindowMode :: (GraphicsReadWrite m, MonadIO m) => WindowMode -> m ()
setGraphicsWindowMode winMode = do
    currentResolution <- getGraphicsResolution

    window <- _window <$> getGraphics
    let
        sdlWinMode = case winMode of
            FullscreenMode        -> SDL.Fullscreen
            FullscreenDesktopMode -> SDL.FullscreenDesktop
            WindowedMode          -> SDL.Windowed
    SDL.setWindowMode window sdlWinMode

    SDL.windowGrab window $= sdlWinMode `elem` [SDL.Fullscreen, SDL.FullscreenDesktop]

    -- sync fullscreen/windowed resolutions if needed
    (currentResolution,) <$> getGraphicsResolution >>= \case
        (Just currentRes, Just res)
            | currentRes /= res -> uncurry setGraphicsResolution currentRes
        _                       -> return ()

    -- recenter window if needed
    when (winMode == WindowedMode) $
        setGraphicsDisplayIndex =<< getGraphicsDisplayIndex

isGraphicsDisplayIndexChanged :: (ConfigsRead m, GraphicsRead m, MonadIO m) => m Bool
isGraphicsDisplayIndexChanged = do
    winDisplayIdx <- readSettingsConfig _render _winDisplayIndex
    (/= winDisplayIdx) <$> getGraphicsDisplayIndex
