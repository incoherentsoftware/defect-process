module Window.Graphics.Fonts
    ( FontType(..)
    , Font
    , mkGraphicsFonts
    , freeGraphicsFonts
    , getGraphicsFont
    , graphicsFontTextWidth
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.Marshal.Alloc  (alloca)
import Foreign.C.String       (withCString)
import Foreign.Ptr            (nullPtr)
import Foreign.Storable       (peek)
import qualified Data.Text as T
import qualified SDL.Font
import qualified SDL.Raw.Font

import Util
import Window.Graphics.Fonts.Types
import Window.Graphics.Types

sysFontFilePath = "data/fonts/Electrolize-Regular.ttf"      :: FilePath
altFontFilePath = "data/fonts/MajorMonoDisplay-Regular.ttf" :: FilePath

fontTypeToSize :: FontType -> Int
fontTypeToSize = \case
    Font12    -> 12
    Font14    -> 14
    Font16    -> 16
    Font22    -> 22
    Font26    -> 26
    Font32    -> 32
    Font44    -> 44
    AltFont36 -> 36

mkGraphicsFonts :: MonadIO m => m GraphicsFonts
mkGraphicsFonts =
    GraphicsFonts <$>
    mkSysFont Font12 <*>
    mkSysFont Font14 <*>
    mkSysFont Font16 <*>
    mkSysFont Font22 <*>
    mkSysFont Font26 <*>
    mkSysFont Font32 <*>
    mkSysFont Font44 <*>
    mkFont altFontFilePath AltFont36 [SDL.Font.Bold]
    where
        mkFont = \filePath fontType styles -> do
            filePath' <- translateResourcePath filePath
            id $
                Font fontType <$>
                SDL.Font.load filePath' (fontTypeToSize fontType) <*>
                pure styles

        mkSysFont = \size -> mkFont sysFontFilePath size []

freeGraphicsFonts :: MonadIO m => GraphicsFonts -> m ()
freeGraphicsFonts graphicsFonts = do
    SDL.Font.free $ _sdlFont (_altFont36 graphicsFonts)
    SDL.Font.free $ _sdlFont (_font44 graphicsFonts)
    SDL.Font.free $ _sdlFont (_font32 graphicsFonts)
    SDL.Font.free $ _sdlFont (_font26 graphicsFonts)
    SDL.Font.free $ _sdlFont (_font22 graphicsFonts)
    SDL.Font.free $ _sdlFont (_font16 graphicsFonts)
    SDL.Font.free $ _sdlFont (_font12 graphicsFonts)

getGraphicsFont :: GraphicsRead m => FontType -> m Font
getGraphicsFont fontType = fontF . _fonts <$> getGraphics
    where
        fontF = case fontType of
            Font12    -> _font12
            Font14    -> _font14
            Font16    -> _font16
            Font22    -> _font22
            Font26    -> _font26
            Font32    -> _font32
            Font44    -> _font44
            AltFont36 -> _altFont36

-- TODO: add cache if this actually gets used
graphicsFontTextWidth :: (GraphicsRead m, MonadIO m) => FontType -> T.Text -> m Int
graphicsFontTextWidth fontType txt = do
    font <- getGraphicsFont fontType
    liftIO . alloca $ \widthPtr ->
        let
            str     = T.unpack txt
            fontPtr = SDL.Font.unwrap $ _sdlFont font
        in do
            ret <- withCString str (\s -> SDL.Raw.Font.sizeText fontPtr s widthPtr nullPtr)
            if
                | ret == 0  -> fromIntegral <$> peek widthPtr
                | otherwise -> return 0
