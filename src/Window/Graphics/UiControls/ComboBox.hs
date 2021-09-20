module Window.Graphics.UiControls.ComboBox
    ( module Window.Graphics.UiControls.ComboBox.Types
    , mkComboBox
    , updateComboBox
    , updateComboBoxEx
    , drawComboBoxEx
    , drawComboBox
    , comboBoxValue
    , isComboBoxCursorHover
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (get, lift, modify, runStateT)
import Data.Foldable          (traverse_)
import Data.Functor           ((<&>))
import Data.Maybe             (fromMaybe, listToMaybe)
import Data.Traversable       (for)
import System.FilePath        (dropExtension)
import qualified Data.Text as T
import qualified SDL

import FileCache.Types
import Util
import Window.Graphics.Color
import Window.Graphics.Fonts
import Window.Graphics.Image
import Window.Graphics.Image.Parse
import Window.Graphics.Primitives
import Window.Graphics.Types
import Window.Graphics.UiControls.Button
import Window.Graphics.UiControls.ComboBox.Types
import Window.Graphics.Util
import Window.InputState

selectedImagePathSuffix = "-selected.image" :: FilePath

valuesOffsetIntervalY = 30.0                  :: PosY
valuesBackdropColor   = Color 100 100 100 255 :: Color

mkComboBox
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => Pos2
    -> T.Text
    -> [T.Text]
    -> FontType
    -> Color
    -> Color
    -> Pos2
    -> PackResourceFilePath
    -> m ComboBox
mkComboBox pos@(Pos2 x y) defaultValue values fontType color hoverColor valueOffset@(Pos2 _ valueOffsetY) imgPath = do
    img         <- loadPackImage imgPath
    selectedImg <- loadPackImage $ imgPath
        { _fileName = dropExtension (_fileName imgPath) ++ selectedImagePathSuffix
        }
    let
        imgWidth  = imageWidth img
        imgHeight = imageHeight img

    valueButton <-
        let
            textArea  = TextButtonAltArea pos imgWidth imgHeight
            valuePos  = pos `vecAdd` valueOffset
        in mkTextButtonLeftAlignEx valuePos defaultValue fontType color hoverColor textArea

    valueButtons <-
        let
            mkTextButton' = \(textPos, text) ->
                let valuePos = textPos `vecAdd` valueOffset
                in mkTextButtonLeftAlign valuePos text fontType color hoverColor

            offsets =
                [ Pos2 x (y + imgHeight - valueOffsetY / 2.0 + fromIntegral i * valuesOffsetIntervalY)
                | i <- [0..]
                ]
        in traverse mkTextButton' (zip offsets values)

    return $ ComboBox
        { _pos           = pos
        , _valueButton   = valueButton
        , _valueButtons  = valueButtons
        , _image         = img
        , _selectedImage = selectedImg
        , _selectionText = defaultValue
        , _isSelected    = False
        , _isPressed     = False
        , _isExpanded    = False
        }

scrollSelectionTextUp :: [Button] -> T.Text -> T.Text
scrollSelectionTextUp valueButtons selectionText = scrollSelectionTextDown (reverse valueButtons) selectionText

scrollSelectionTextDown :: [Button] -> T.Text -> T.Text
scrollSelectionTextDown valueButtons selectionText = search $ valueButtons ++ take 1 valueButtons
    where
        search = \case
            [] -> case listToMaybe valueButtons of
                Just btn -> fromMaybe selectionText (buttonText btn)
                Nothing  -> selectionText

            (btn:btns)
                | maybe False (== selectionText) (buttonText btn) ->
                    fromMaybe selectionText (buttonText =<< listToMaybe btns)
                | otherwise                                       -> search btns

updateComboBox :: InputRead m => ComboBoxStatus -> T.Text -> ComboBox -> m ComboBox
updateComboBox status valueText comboBox = do
    isClick <- (SDL.ButtonLeft `mousePressed`) <$> readInputState
    updateComboBoxEx status valueText isClick comboBox

updateComboBoxEx :: InputRead m => ComboBoxStatus -> T.Text -> Bool -> ComboBox -> m ComboBox
updateComboBoxEx status valueText isClick comboBox = do
    inputState    <- readInputState
    let isExpanded = _isExpanded comboBox

    valueBtn <-
        let
            btnStatus
                | isExpanded = ButtonInactiveStatus
                | otherwise  = case status of
                    ComboBoxSelectedActiveStatus -> ButtonSelectedActiveStatus
                    ComboBoxActiveStatus         -> ButtonActiveStatus
                    ComboBoxInactiveStatus       -> ButtonInactiveStatus
        in updateButton btnStatus (updateButtonText valueText (_valueButton comboBox))

    let
        selectionText
            | isExpanded = _selectionText comboBox
            | otherwise  = valueText

        isValueButtonHover :: Button -> Bool
        isValueButtonHover btn = isExpanded && inVerticalBounds && inHorizontalBounds
            where
                btnPosY            = vecY $ _pos (btn :: Button)
                Pos2 mouseX mouseY = _mousePos inputState
                inVerticalBounds   = mouseY >= btnPosY && mouseY <= btnPosY + _height btn
                posX               = vecX $ _pos (comboBox :: ComboBox)
                inHorizontalBounds = mouseX >= posX && mouseX <= posX + imageWidth (_image comboBox)

    (valueBtns, selectionText') <- flip runStateT selectionText $ do
        let btns = _valueButtons comboBox

        when (isExpanded && MenuUpAlias `aliasPressed` inputState) $
            modify $ scrollSelectionTextUp btns
        when (isExpanded && MenuDownAlias `aliasPressed` inputState) $
            modify $ scrollSelectionTextDown btns

        for btns $ \btn -> do
            isBtnTextMatch <- get <&> \txt -> maybe False (== txt) (buttonText btn)
            let
                mouseMoved                        = _mouseMoved inputState
                isBtnSelected                     = isBtnTextMatch || (isValueButtonHover btn && mouseMoved)
                btnStatus
                    | isExpanded && isBtnSelected = ButtonSelectedActiveStatus
                    | isExpanded                  = ButtonActiveStatus
                    | otherwise                   = ButtonInactiveStatus

            btn' <- lift $ updateButtonEx btnStatus isClick btn
            when (_isPressed (btn' :: Button) || isBtnSelected) $
                modify $ \txt -> fromMaybe txt (buttonText btn')
            return btn'

    let
        isMenuSelectPressed = MenuSelectAlias `aliasPressed` inputState
        isMenuOrBackPressed = MenuAlias `aliasPressed` inputState || MenuBackAlias `aliasPressed` inputState

        isValueButtonPressed :: Button -> Bool
        isValueButtonPressed btn = isPressed || isPressedByMouse || isPressedByKey
            where
                isPressed        = _isPressed (btn :: Button)
                isPressedByMouse = isValueButtonHover btn && isClick
                isPressedByKey   = _isSelected (btn :: Button) && isMenuSelectPressed

        (valueBtn', isAnyValueBtnsPressed) = case filter isValueButtonPressed valueBtns of
            []      -> (valueBtn, False)
            (btn:_) -> (maybe valueBtn (\t -> updateButtonText t valueBtn) (buttonText btn), True)

        isExpanded'
            | isExpanded = not isClick && not isMenuSelectPressed && not isMenuOrBackPressed
            | otherwise  = _isPressed (valueBtn' :: Button)

    return $ (comboBox :: ComboBox)
        { _valueButton   = valueBtn'
        , _valueButtons  = valueBtns
        , _selectionText = selectionText'
        , _isSelected    = _isSelected (valueBtn' :: Button)
        , _isPressed     = (isExpanded' && not isExpanded) || isAnyValueBtnsPressed
        , _isExpanded    = isExpanded'
        }

drawComboBoxEx :: (GraphicsReadWrite m, MonadIO m) => ZIndex -> ZIndex -> Bool -> ComboBox -> m ()
drawComboBoxEx zIndex expandedZIndex showValueButton comboBox =
    let
        pos        = _pos (comboBox :: ComboBox)
        valueBtns  = _valueButtons comboBox
        isExpanded = _isExpanded comboBox
    in do
        when isExpanded $
            let
                img       = _image comboBox
                imgWidth  = imageWidth img
                imgHeight = imageHeight img
                rectPos   = pos `vecAdd` Pos2 0.0 imgHeight
                height    = fromIntegral (length valueBtns) * valuesOffsetIntervalY
            in do
                drawRect rectPos imgWidth height valuesBackdropColor expandedZIndex
                traverse_ (drawButton expandedZIndex) valueBtns

        when showValueButton $
            let
                valueBtn = case filter (_isSelected :: Button -> Bool) valueBtns of
                    (btn:_)
                        | isExpanded -> (btn :: Button)
                            { _pos        = _pos (_valueButton comboBox :: Button)
                            , _isSelected = False
                            }
                    _                -> _valueButton comboBox
            in drawButton zIndex valueBtn

        let
            isSelected                         = _isSelected (comboBox :: ComboBox)
            img
                | isSelected && not isExpanded = _selectedImage comboBox
                | otherwise                    = _image comboBox
        drawImage pos RightDir zIndex img

drawComboBox :: (GraphicsReadWrite m, MonadIO m) => ZIndex -> ZIndex -> ComboBox -> m ()
drawComboBox zIndex expandedZIndex comboBox = drawComboBoxEx zIndex expandedZIndex True comboBox

comboBoxValue :: ComboBox -> T.Text
comboBoxValue comboBox = fromMaybe "" (buttonText (_valueButton comboBox))

isComboBoxCursorHover :: InputRead m => ComboBox -> m Bool
isComboBoxCursorHover = isButtonCursorHover . _valueButton
