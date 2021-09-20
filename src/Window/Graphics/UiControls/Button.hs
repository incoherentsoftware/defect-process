module Window.Graphics.UiControls.Button
    ( module Window.Graphics.UiControls.Button.Types
    , ButtonStatus(..)
    , mkTextButton
    , mkTextButtonTopLeftAlign
    , mkTextButtonLeftAlign
    , mkTextButtonLeftAlignEx
    , mkImageButtonEx
    , mkImageButton
    , mkImageButtonCentered
    , mkImageButtonTopCentered
    , updateButtonEx
    , updateButton
    , drawButton
    , updateButtonText
    , setButtonSelectedPressed
    , buttonText
    , isButtonCursorHover
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import System.FilePath        (dropExtension)
import qualified Data.Text as T
import qualified SDL

import FileCache.Types
import Util
import Window.Graphics.Color
import Window.Graphics.DisplayText
import Window.Graphics.Fonts
import Window.Graphics.Image
import Window.Graphics.Image.Parse
import Window.Graphics.Types
import Window.Graphics.UiControls.Button.Types
import Window.Graphics.Util
import Window.InputState

selectedImagePathSuffix = "-selected.image" :: FilePath

data Alignment
    = CenterAlign
    | TopLeftAlign
    | LeftAlign

mkTextButton :: (GraphicsRead m, MonadIO m) => Pos2 -> T.Text -> FontType -> Color -> Color -> m Button
mkTextButton pos text fontType color hoverColor = do
    mkTextButtonInternal pos text fontType color hoverColor CenterAlign Nothing

mkTextButtonTopLeftAlign :: (GraphicsRead m, MonadIO m) => Pos2 -> T.Text -> FontType -> Color -> Color -> m Button
mkTextButtonTopLeftAlign pos text fontType color hoverColor =
    mkTextButtonInternal pos text fontType color hoverColor TopLeftAlign Nothing

mkTextButtonLeftAlign :: (GraphicsRead m, MonadIO m) => Pos2 -> T.Text -> FontType -> Color -> Color -> m Button
mkTextButtonLeftAlign pos text fontType color hoverColor =
    mkTextButtonInternal pos text fontType color hoverColor LeftAlign Nothing

mkTextButtonLeftAlignEx
    :: (GraphicsRead m, MonadIO m)
    => Pos2
    -> T.Text
    -> FontType
    -> Color
    -> Color
    -> TextButtonArea
    -> m Button
mkTextButtonLeftAlignEx pos text fontType color hoverColor textArea =
    mkTextButtonInternal pos text fontType color hoverColor LeftAlign (Just textArea)

mkTextButtonInternal
    :: (GraphicsRead m, MonadIO m)
    => Pos2
    -> T.Text
    -> FontType
    -> Color
    -> Color
    -> Alignment
    -> Maybe TextButtonArea
    -> m Button
mkTextButtonInternal (Pos2 x y) text fontType color hoverColor alignment textArea = do
    displayText      <- mkDisplayText text fontType color
    width            <- displayTextWidth displayText
    height           <- displayTextHeight displayText
    hoverDisplayText <- mkDisplayText text fontType hoverColor

    let
        centerX = x - width / 2.0
        centerY = y - height / 2.0
        pos     = case alignment of
            CenterAlign  -> Pos2 centerX centerY
            TopLeftAlign -> Pos2 x y
            LeftAlign    -> Pos2 x centerY

        area    = fromMaybe (TextButtonArea width height) textArea
        btnType = TextButtonType area displayText hoverDisplayText

    mkButton pos btnType

toSelectedImagePackPath :: PackResourceFilePath -> PackResourceFilePath
toSelectedImagePackPath imgPath = imgPath {_fileName = selectedImgFileName}
    where selectedImgFileName = dropExtension (_fileName imgPath) ++ selectedImagePathSuffix

mkImageButtonEx
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => Pos2
    -> PackResourceFilePath
    -> PackResourceFilePath
    -> m Button
mkImageButtonEx pos imgPath selectedImgPath = do
    img         <- loadPackImage imgPath
    selectedImg <- loadPackImage selectedImgPath
    mkButton pos (ImageButtonType img selectedImg)

mkImageButton :: (FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> PackResourceFilePath -> m Button
mkImageButton pos imgPath = mkImageButtonEx pos imgPath (toSelectedImagePackPath imgPath)

mkImageButtonInternal
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => Pos2
    -> Image
    -> PackResourceFilePath
    -> m Button
mkImageButtonInternal pos img selectedImgPath = do
    selectedImg <- loadPackImage selectedImgPath
    mkButton pos (ImageButtonType img selectedImg)

mkImageButtonCentered :: (FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> PackResourceFilePath -> m Button
mkImageButtonCentered (Pos2 x y) imgPath = do
    img    <- loadPackImage imgPath
    let pos = Pos2 (x - imageWidth img / 2.0) (y - imageHeight img / 2.0)
    mkImageButtonInternal pos img (toSelectedImagePackPath imgPath)

mkImageButtonTopCentered :: (FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> PackResourceFilePath -> m Button
mkImageButtonTopCentered (Pos2 x y) imgPath = do
    img    <- loadPackImage imgPath
    let pos = Pos2 (x - imageWidth img / 2.0) y
    mkImageButtonInternal pos img (toSelectedImagePackPath imgPath)

mkButton :: (GraphicsRead m, MonadIO m) => Pos2 -> ButtonType -> m Button
mkButton pos btnType = do
    btnHeight <- case btnType of
        ImageButtonType img _         -> return $ imageHeight img
        TextButtonType _ displayTxt _ -> displayTextHeight displayTxt

    return $ Button
        { _type       = btnType
        , _pos        = pos
        , _isSelected = False
        , _isPressed  = False
        , _height     = btnHeight
        }

isButtonStatusActive :: ButtonStatus -> Bool
isButtonStatusActive = \case
    ButtonSelectedActiveStatus -> True
    ButtonActiveStatus         -> True
    ButtonInactiveStatus       -> False

updateButtonEx :: InputRead m => ButtonStatus -> Bool -> Button -> m Button
updateButtonEx btnStatus isClick btn = do
    isCursorHover <- isButtonCursorHover btn
    inputState    <- readInputState

    let
        isSelectedActive                     = btnStatus == ButtonSelectedActiveStatus
        isPressed
            | isButtonStatusActive btnStatus =
                let isSelectedPressed = isSelectedActive && MenuSelectAlias `aliasPressed` inputState
                in (isCursorHover && isClick) || isSelectedPressed
            | otherwise                      = False
        isInactive                           = btnStatus == ButtonInactiveStatus

    return $ btn
        { _isSelected = isSelectedActive || (isCursorHover && _mouseMoved inputState && not isInactive)
        , _isPressed  = isPressed
        }

updateButton :: InputRead m => ButtonStatus -> Button -> m Button
updateButton btnStatus btn = do
    isClick <- (SDL.ButtonLeft `mousePressed`) <$> readInputState
    updateButtonEx btnStatus isClick btn

setButtonSelectedPressed :: Button -> Button
setButtonSelectedPressed btn = btn
    { _isSelected = True
    , _isPressed  = True
    }

drawButton :: (GraphicsReadWrite m, MonadIO m) => ZIndex -> Button -> m ()
drawButton zIndex btn =
    let
        pos      = _pos btn
        selected = _isSelected btn
    in case _type btn of
        TextButtonType _ displayText hoverDisplayText ->
            let drwText = if selected then hoverDisplayText else displayText
            in drawDisplayText pos zIndex drwText

        ImageButtonType image selectedImage ->
            let img = if selected then selectedImage else image
            in drawImage pos RightDir zIndex img

updateButtonText :: T.Text -> Button -> Button
updateButtonText text btn = case _type btn of
    ImageButtonType _ _                            -> btn
    TextButtonType area displayTxt hoverDisplayTxt ->
        let
            displayTxt'      = updateDisplayText text displayTxt
            hoverDisplayTxt' = updateDisplayText text hoverDisplayTxt
        in btn {_type = TextButtonType area displayTxt' hoverDisplayTxt'}

buttonText :: Button -> Maybe T.Text
buttonText btn = case _type btn of
    ImageButtonType _ _           -> Nothing
    TextButtonType _ displayTxt _ -> Just $ _text displayTxt

isButtonCursorHover :: InputRead m => Button -> m Bool
isButtonCursorHover btn = isHover <$> readInputState
    where
        isHover :: InputState -> Bool
        isHover inputState = cursorX >= btnX && cursorX <= btnX + btnW && cursorY >= btnY && cursorY <= btnY + btnH
            where
                Pos2 cursorX cursorY         = _mousePos inputState
                btnPos                       = _pos btn
                (Pos2 btnX btnY, btnW, btnH) = case _type btn of
                    ImageButtonType img _   -> (btnPos, imageWidth img, imageHeight img)
                    TextButtonType area _ _ -> case area of
                        TextButtonArea w h           -> (btnPos, w, h)
                        TextButtonAltArea altPos w h -> (altPos, w, h)
