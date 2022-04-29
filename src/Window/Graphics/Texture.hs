module Window.Graphics.Texture
    ( module Window.Graphics.Texture.Types
    , loadTexture
    , loadTextureEx
    , mkTexture
    , drawTextureSubRectEx
    , drawTexture
    , drawTextureScaled
    , drawTextureScaledEx
    , freeTextures
    ) where

import Control.Concurrent.STM      (atomically)
import Control.Concurrent.STM.TVar (readTVarIO, writeTVar)
import Control.Monad.IO.Class      (MonadIO, liftIO)
import Data.IORef                  (readIORef)
import Data.StateVar               (($=))
import Foreign.C.Types             (CDouble(CDouble))
import qualified Data.Set as S
import qualified SDL
import qualified SDL.Image

import Id
import Util
import Window.Graphics.BlendMode
import Window.Graphics.Camera
import Window.Graphics.DrawCall
import Window.Graphics.Opacity
import Window.Graphics.Renderer
import Window.Graphics.SubRect
import Window.Graphics.Texture.Manager
import Window.Graphics.Texture.Types
import Window.Graphics.Types
import Window.Graphics.Util

loadTexture :: (GraphicsRead m, MonadIO m) => FilePath -> m Texture
loadTexture filePath = loadTextureInternal filePath Nothing

loadTextureEx :: (GraphicsRead m, MonadIO m) => FilePath -> SDL.Surface -> m Texture
loadTextureEx filePath sdlSurface = loadTextureInternal filePath (Just sdlSurface)

loadTextureInternal :: (GraphicsRead m, MonadIO m) => FilePath -> Maybe SDL.Surface -> m Texture
loadTextureInternal filePath sdlSurface = do
    gfx              <- getGraphics
    let textureMgrVar = _textureManagerVar gfx
    textureMgr       <- liftIO $ readTVarIO textureMgrVar

    case getTextureManagerTexture filePath textureMgr of
        Just texture -> return texture
        Nothing      -> do
            sdlSurface'     <- maybe (SDL.Image.load =<< translateResourcePath filePath) return sdlSurface
            (width, height) <- surfaceWidthHeight sdlSurface'
            sdlTexture      <- SDL.createTextureFromSurface (_sdlRenderer $ _renderer gfx) sdlSurface'
            SDL.freeSurface sdlSurface'

            texture        <- mkTexture sdlTexture width height
            let textureMgr' = putTextureManagerTexture filePath texture textureMgr
            liftIO . atomically $ writeTVar textureMgrVar textureMgr'

            return texture

mkTexture :: MonadIO m => SDL.Texture -> Int -> Int -> m Texture
mkTexture sdlTexture width height = do
    SDL.textureBlendMode sdlTexture $= SDL.BlendAlphaBlend
    textureId <- newId
    return $ Texture
        { _id         = textureId
        , _sdlTexture = sdlTexture
        , _width      = width
        , _height     = height
        }

drawTextureSubRectEx
    :: (GraphicsRead m, MonadIO m)
    => SubRect
    -> Pos2
    -> Pos2
    -> Float
    -> Float
    -> Direction
    -> ZIndex
    -> Radians
    -> Opacity
    -> DrawScale
    -> Texture
    -> m ()
drawTextureSubRectEx srcSubRect originPos pos destWidth destHeight dir zIndex angle opacity drawScale texture =
    let
        sdlTexture           = _sdlTexture texture
        alpha                = opacityToAlpha opacity
        width                = fromIntegral $ _width (srcSubRect :: SubRect)
        flipX                = dir == LeftDir
        Pos2 originX originY = originPos
        originX'             = if flipX then width - originX else originX
        originXOffset        = if flipX then originX' else originX
        originYOffset        = originY
        angle'               = CDouble $ realToFrac (toDegrees angle)
        rotatePoint          = Just . SDL.P $ SDL.V2 (realToFrac originX') (realToFrac originY)
        flipXY               = if flipX then SDL.V2 True False else SDL.V2 False False

        subRectPoint = SDL.P $ SDL.V2 (_x srcSubRect) (_y srcSubRect)
        subRectSize  = SDL.V2 (_width (srcSubRect :: SubRect)) (_height (srcSubRect :: SubRect))
        srcRect      = Just $ SDL.Rectangle subRectPoint subRectSize

        scale       = drawScaleToFloat drawScale
        destWidth'  = realToFrac $ destWidth * scale
        destHeight' = realToFrac $ destHeight * scale
        destSize    = SDL.V2 destWidth' destHeight'
    in do
        gfx       <- getGraphics
        blendMode <- liftIO $ readIORef (_blendModeRef gfx)
        destRect  <- cameraTransformPos pos >>= \(Pos2 x' y') ->
            let
                destX     = realToFrac $ x' - originXOffset * scale
                destY     = realToFrac $ y' - originYOffset * scale
                destPoint = SDL.P $ SDL.V2 destX destY
            in return . Just $ SDL.Rectangle destPoint destSize

        addGraphicsDrawCall zIndex $ do
            case blendMode of
                BlendModeAlpha    -> return ()
                BlendModeAdditive -> SDL.textureBlendMode sdlTexture $= SDL.BlendAdditive

            SDL.textureAlphaMod sdlTexture $= alpha
            let sdlRenderer = _sdlRenderer $ _renderer gfx
            SDL.copyExF sdlRenderer sdlTexture srcRect destRect angle' rotatePoint flipXY

            case blendMode of
                BlendModeAlpha    -> return ()
                BlendModeAdditive -> SDL.textureBlendMode sdlTexture $= SDL.BlendAlphaBlend

drawTexture :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> Pos2 -> Direction -> ZIndex -> Texture -> m ()
drawTexture originPos pos dir zIndex texture =
    drawTextureScaledEx originPos pos dir NonScaled zIndex 0.0 FullOpacity texture

drawTextureScaled
    :: (GraphicsReadWrite m, MonadIO m)
    => Pos2
    -> Pos2
    -> Direction
    -> DrawScale
    -> ZIndex
    -> Texture
    -> m ()
drawTextureScaled originPos pos dir drawScale zIndex texture =
    drawTextureScaledEx originPos pos dir drawScale zIndex 0.0 FullOpacity texture

drawTextureScaledEx
    :: (GraphicsReadWrite m, MonadIO m)
    => Pos2
    -> Pos2
    -> Direction
    -> DrawScale
    -> ZIndex
    -> Radians
    -> Opacity
    -> Texture
    -> m ()
drawTextureScaledEx originPos pos dir drawScale zIndex angle opacity texture =
    drawTextureSubRectEx subRect originPos pos destWidth destHeight dir zIndex angle opacity drawScale texture
        where
            subRect = SubRect
                { _x      = 0
                , _y      = 0
                , _width  = fromIntegral $ _width (texture :: Texture)
                , _height = fromIntegral $ _height (texture :: Texture)
                }

            destWidth  = realToFrac $ _width (subRect :: SubRect)
            destHeight = realToFrac $ _height (subRect :: SubRect)

freeTextures :: (GraphicsReadWrite m, MonadIO m) => S.Set Texture -> m ()
freeTextures textures = do
    gfx              <- getGraphics
    let textureMgrVar = _textureManagerVar gfx
    textureMgr       <- liftIO $ readTVarIO textureMgrVar
    textureMgr'      <- freeTextureManagerTextures textures textureMgr
    liftIO . atomically $ writeTVar textureMgrVar textureMgr'
