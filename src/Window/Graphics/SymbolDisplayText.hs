module Window.Graphics.SymbolDisplayText
    ( SymbolDisplayText
    , imageReplacementSpacesText
    , parseLoadPrefixSymbolImage
    , mkSymbolDisplayText
    , updateSymbolDisplayText
    , drawSymbolDisplayText
    , drawSymbolDisplayTextCentered
    , drawSymbolDisplayTextRightAligned
    , drawSymbolDisplayTextRightAlignedEx
    , symbolDisplayTextWidth
    , symbolDisplayTextHeight
    , symbolDisplayTextImageWidth
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor         (bimap)
import qualified Data.Text as T
import qualified SDL.Font

import FileCache
import Util
import Window.Graphics.Color
import Window.Graphics.DisplayText
import Window.Graphics.Fonts
import Window.Graphics.Fonts.Types
import Window.Graphics.Image
import Window.Graphics.Image.Parse
import Window.Graphics.Opacity
import Window.Graphics.SymbolDisplayText.Types
import Window.Graphics.Types
import Window.Graphics.Util

uiPackPath                         = \f -> PackResourceFilePath "data/ui/ui.pack" f
goldSymbolImagePath                = uiPackPath "symbol-gold.image"                  :: PackResourceFilePath
unlocksCreditsSymbolImagePath      = uiPackPath "symbol-unlocks-credits.image"       :: PackResourceFilePath
unlocksCreditsLargeSymbolImagePath = uiPackPath "symbol-unlocks-credits-large.image" :: PackResourceFilePath
jumpSymbolImagePath                = uiPackPath "symbol-jump.image"                  :: PackResourceFilePath
weaponSymbolImagePath              = uiPackPath "symbol-weapon.image"                :: PackResourceFilePath
shootSymbolImagePath               = uiPackPath "symbol-shoot.image"                 :: PackResourceFilePath
movementSkillSymbolImagePath       = uiPackPath "symbol-movement-skill.image"        :: PackResourceFilePath
secondarySkillSymbolImagePath      = uiPackPath "symbol-secondary-skill.image"       :: PackResourceFilePath

secondarySkillNeutralInputSymbolImagePath =
    uiPackPath "symbol-secondary-skill-neutral-input.image" :: PackResourceFilePath
secondarySkillUpInputSymbolImagePath      =
    uiPackPath "symbol-secondary-skill-up-input.image"      :: PackResourceFilePath
secondarySkillDownInputSymbolImagePath    =
    uiPackPath "symbol-secondary-skill-down-input.image"    :: PackResourceFilePath

imageReplacementSpacesText :: MonadIO m => Image -> Font -> m T.Text
imageReplacementSpacesText img font = do
    spaceWidth   <- fromIntegral . fst <$> SDL.Font.size (_sdlFont font) " "
    let numSpaces = ceiling $ imageWidth img / spaceWidth
    return $ T.replicate numSpaces " "

parseLoadPrefixSymbolImage :: (FileCache m, GraphicsRead m, MonadIO m) => T.Text -> Font -> m (Maybe Image, T.Text)
parseLoadPrefixSymbolImage txt font = case specialTxt of
    "{GoldSymbol}"                       -> symbol goldSymbolImagePath
    "{UnlocksCreditsSymbol}"             -> symbol unlocksCreditsSymbolImagePath
    "{UnlocksCreditsLargeSymbol}"        -> symbol unlocksCreditsLargeSymbolImagePath
    "{JumpSymbol}"                       -> symbol jumpSymbolImagePath
    "{WeaponSymbol}"                     -> symbol weaponSymbolImagePath
    "{ShootSymbol}"                      -> symbol shootSymbolImagePath
    "{MovementSkillSymbol}"              -> symbol movementSkillSymbolImagePath
    "{SecondarySkillSymbol}"             -> symbol secondarySkillSymbolImagePath
    "{SecondarySkillNeutralInputSymbol}" -> symbol secondarySkillNeutralInputSymbolImagePath
    "{SecondarySkillUpInputSymbol}"      -> symbol secondarySkillUpInputSymbolImagePath
    "{SecondarySkillDownInputSymbol}"    -> symbol secondarySkillDownInputSymbolImagePath
    _                                    -> return (Nothing, txt)
    where
        (specialTxt, remainderTxt)
            | "{" `T.isPrefixOf` txt = bimap (<> "}") (T.drop 1) (T.breakOn "}" txt)
            | otherwise              = (T.empty, txt)

        symbol = \imgPath -> do
            img    <- loadPackImage imgPath
            spaces <- imageReplacementSpacesText img font
            return (Just img, spaces <> remainderTxt)

mkSymbolDisplayText :: (FileCache m, GraphicsRead m, MonadIO m) => T.Text -> FontType -> Color -> m SymbolDisplayText
mkSymbolDisplayText txt fontType color = do
    font                         <- getGraphicsFont fontType
    (prefixSymbolImg, parsedTxt) <- parseLoadPrefixSymbolImage txt font
    displayTxt                   <- mkDisplayText parsedTxt fontType color

    return $ SymbolDisplayText
        { _displayText       = displayTxt
        , _text              = txt
        , _prefixSymbolImage = prefixSymbolImg
        }

updateSymbolDisplayText
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => T.Text
    -> SymbolDisplayText
    -> m SymbolDisplayText
updateSymbolDisplayText txt symbolDisplayTxt
    | txt == _text (symbolDisplayTxt :: SymbolDisplayText) = return symbolDisplayTxt
    | otherwise                                            =
        let
            displayTxt = _displayText symbolDisplayTxt
            fontType   = _type $ _font displayTxt
        in mkSymbolDisplayText txt fontType (_color displayTxt)

drawPrefixSymbolImage :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> ZIndex -> SymbolDisplayText -> m ()
drawPrefixSymbolImage pos zIndex symbolDisplayTxt = case _prefixSymbolImage symbolDisplayTxt of
    Nothing  -> return ()
    Just img -> do
        txtHeight <- displayTextHeight $ _displayText symbolDisplayTxt
        let
            offset = Pos2 (imageWidth img / 2.0) (txtHeight / 2.0)
            pos'   = vecFloorXY $ pos `vecAdd` offset
        drawImage pos' RightDir zIndex img

drawSymbolDisplayText :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> ZIndex -> SymbolDisplayText -> m ()
drawSymbolDisplayText pos zIndex symbolDisplayTxt = do
    drawDisplayText pos zIndex (_displayText symbolDisplayTxt)
    drawPrefixSymbolImage pos zIndex symbolDisplayTxt

drawSymbolDisplayTextCentered :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> ZIndex -> SymbolDisplayText -> m ()
drawSymbolDisplayTextCentered (Pos2 x y) zIndex symbolDisplayTxt = do
    width  <- symbolDisplayTextWidth symbolDisplayTxt
    height <- symbolDisplayTextHeight symbolDisplayTxt
    let pos = Pos2 (x - width / 2.0) (y - height / 2.0)
    drawSymbolDisplayText pos zIndex symbolDisplayTxt

drawSymbolDisplayTextRightAligned :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> ZIndex -> SymbolDisplayText -> m ()
drawSymbolDisplayTextRightAligned pos zIndex symbolDisplayTxt =
    drawSymbolDisplayTextRightAlignedEx pos zIndex NonScaled FullOpacity symbolDisplayTxt

drawSymbolDisplayTextRightAlignedEx
    :: (GraphicsReadWrite m, MonadIO m)
    => Pos2
    -> ZIndex
    -> DrawScale
    -> Opacity
    -> SymbolDisplayText
    -> m ()
drawSymbolDisplayTextRightAlignedEx pos@(Pos2 x y) zIndex scale opacity symbolDisplayTxt = do
    drawDisplayTextRightAlignedEx pos zIndex scale opacity (_displayText symbolDisplayTxt)

    width       <- displayTextWidth $ _displayText symbolDisplayTxt
    let prefixPos = Pos2 (x - width) y
    drawPrefixSymbolImage prefixPos zIndex symbolDisplayTxt

symbolDisplayTextWidth :: (GraphicsRead m, MonadIO m) => SymbolDisplayText -> m Float
symbolDisplayTextWidth = displayTextWidth . _displayText

symbolDisplayTextHeight :: (GraphicsRead m, MonadIO m) => SymbolDisplayText -> m Float
symbolDisplayTextHeight = displayTextHeight . _displayText

symbolDisplayTextImageWidth :: SymbolDisplayText -> Float
symbolDisplayTextImageWidth symbolDisplayTxt = maybe 0.0 imageWidth (_prefixSymbolImage symbolDisplayTxt)
