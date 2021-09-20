module Window.Graphics.DisplayText
    ( module Window.Graphics.DisplayText.Types
    , mkDisplayText
    , displayTextWidth
    , displayTextHeight
    , appendDisplayText
    , updateDisplayText
    , drawDisplayText
    , drawDisplayTextEx
    , drawDisplayTextCentered
    , drawDisplayTextCenteredEx
    , drawDisplayTextRightAlignedEx
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef             (newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import qualified SDL
import qualified SDL.Font

import Util
import Window.Graphics.Color
import Window.Graphics.DisplayText.Types
import Window.Graphics.Fonts
import Window.Graphics.Fonts.Types
import Window.Graphics.Opacity
import Window.Graphics.Renderer
import Window.Graphics.Texture
import Window.Graphics.Types
import Window.Graphics.Util

mkFontTexture :: (GraphicsRead m, MonadIO m) => T.Text -> Font -> Color -> m Texture
mkFontTexture text font color =
    let
        sdlFont       = _sdlFont font
        sdlFontStyles = _sdlFontStyles font
        text'         = if T.null text then " " else text  -- SDL.Font will crash if the text is 0 width
    in do
        SDL.Font.setStyle sdlFont sdlFontStyles
        surface         <- SDL.Font.blended sdlFont (colorToV4 color) text'
        (width, height) <- surfaceWidthHeight surface
        sdlRenderer     <- (_sdlRenderer . _renderer) <$> getGraphics
        sdlTexture      <- SDL.createTextureFromSurface sdlRenderer surface
        SDL.freeSurface surface
        mkTexture sdlTexture width height

mkDisplayText :: (GraphicsRead m, MonadIO m) => T.Text -> FontType -> Color -> m DisplayText
mkDisplayText text fontType color = do
    font           <- getGraphicsFont fontType
    textureRef     <- liftIO . newIORef =<< mkFontTexture text font color
    textureTextRef <- liftIO $ newIORef text

    return $ DisplayText
        { _text           = text
        , _font           = font
        , _color          = color
        , _textureRef     = textureRef
        , _textureTextRef = textureTextRef
        }

displayTextWidth :: (GraphicsRead m, MonadIO m) => DisplayText -> m Float
displayTextWidth displayTxt = do
    updateDisplayTextTexture displayTxt
    (fromIntegral . _width) <$> liftIO (readIORef $ _textureRef displayTxt)

displayTextHeight :: (GraphicsRead m, MonadIO m) => DisplayText -> m Float
displayTextHeight displayTxt = do
    updateDisplayTextTexture displayTxt
    (fromIntegral . _height) <$> liftIO (readIORef $ _textureRef displayTxt)

appendDisplayText :: T.Text -> DisplayText -> DisplayText
appendDisplayText text displayTxt = displayTxt {_text = text'}
    where text' = _text displayTxt `T.append` text

updateDisplayText :: T.Text -> DisplayText -> DisplayText
updateDisplayText text displayTxt = displayTxt {_text = text}

updateDisplayTextTexture :: (GraphicsRead m, MonadIO m) => DisplayText -> m ()
updateDisplayTextTexture displayTxt =
    let
        font           = _font displayTxt
        text           = _text displayTxt
        color          = _color displayTxt
        textureRef     = _textureRef displayTxt
        textureTextRef = _textureTextRef displayTxt
    in do
        textureText <- liftIO $ readIORef textureTextRef
        when (textureText /= text) $ do
            texture' <- mkFontTexture text font color
            liftIO $ writeIORef textureRef texture'
            liftIO $ writeIORef textureTextRef text

drawDisplayText :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> ZIndex -> DisplayText -> m ()
drawDisplayText pos zIndex displayTxt = drawDisplayTextEx pos zIndex NonScaled FullOpacity displayTxt

drawDisplayTextEx :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> ZIndex -> DrawScale -> Opacity -> DisplayText -> m ()
drawDisplayTextEx pos zIndex scale opacity displayTxt = do
    updateDisplayTextTexture displayTxt
    texture <- liftIO $ readIORef (_textureRef displayTxt)
    drawTextureScaledEx zeroPos2 pos RightDir scale zIndex 0.0 opacity texture

drawDisplayTextCentered :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> ZIndex -> DisplayText -> m ()
drawDisplayTextCentered pos zIndex displayTxt = drawDisplayTextCenteredEx pos zIndex NonScaled FullOpacity displayTxt

drawDisplayTextCenteredEx
    :: (GraphicsReadWrite m, MonadIO m)
    => Pos2
    -> ZIndex
    -> DrawScale
    -> Opacity
    -> DisplayText
    -> m ()
drawDisplayTextCenteredEx (Pos2 x y) zIndex scale opacity displayTxt = do
    width  <- displayTextWidth displayTxt
    height <- displayTextHeight displayTxt
    let pos = Pos2 (x - width / 2.0) (y - height / 2.0)
    drawDisplayTextEx pos zIndex scale opacity displayTxt

drawDisplayTextRightAlignedEx
    :: (GraphicsReadWrite m, MonadIO m)
    => Pos2
    -> ZIndex
    -> DrawScale
    -> Opacity
    -> DisplayText
    -> m ()
drawDisplayTextRightAlignedEx (Pos2 x y) zIndex scale opacity displayTxt = do
    width  <- displayTextWidth displayTxt
    let pos = Pos2 (x - width) y
    drawDisplayTextEx pos zIndex scale opacity displayTxt
