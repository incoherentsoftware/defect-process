module Window.Graphics.InputDisplayText
    ( InputDisplayText(_text)
    , mkInputDisplayText
    , updateInputDisplayText
    , drawInputDisplayText
    , drawInputDisplayTextMouseKb
    , drawInputDisplayTextGamepad
    , drawInputDisplayTextCentered
    , drawInputDisplayTextMouseKbCentered
    , drawInputDisplayTextGamepadCentered
    , inputDisplayTextWidth
    , inputDisplayTextHeight
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (for_)
import Data.Functor           ((<&>))
import Data.Maybe             (isNothing, mapMaybe)
import Text.Printf            (printf)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified SDL.Font
import qualified SDL.Raw

import FileCache
import Util
import Window.Graphics.Color
import Window.Graphics.Fonts
import Window.Graphics.Fonts.Types
import Window.Graphics.Image
import Window.Graphics.Image.Parse
import Window.Graphics.InputDisplayText.Types
import Window.Graphics.SymbolDisplayText
import Window.Graphics.Texture
import Window.Graphics.Types
import Window.Graphics.Util
import Window.InputState.Alias
import Window.InputState.GamepadManager.Types
import Window.InputState.RawData
import Window.InputState.Types

separatorText = " / " :: T.Text

specialTextToInputAlias :: T.Text -> Maybe InputAlias
specialTextToInputAlias = \case
    "{LeftAlias}"               -> Just LeftAlias
    "{RightAlias}"              -> Just RightAlias
    "{DownAlias}"               -> Just DownAlias
    "{UpAlias}"                 -> Just UpAlias
    "{JumpAlias}"               -> Just JumpAlias
    "{WeaponAlias}"             -> Just WeaponAlias
    "{ShootAlias}"              -> Just ShootAlias
    "{SwitchWeaponAlias}"       -> Just SwitchWeaponAlias
    "{SwitchGunAlias}"          -> Just SwitchGunAlias
    "{InteractAlias}"           -> Just InteractAlias
    "{MovementSkillAlias}"      -> Just MovementSkillAlias
    "{SecondarySkillAlias}"     -> Just SecondarySkillAlias
    "{LockOnCursorAlias}"       -> Just LockOnCursorAlias
    "{LockOnClearAlias}"        -> Just LockOnClearAlias
    "{LockOnSwitchTargetAlias}" -> Just LockOnSwitchTargetAlias
    "{MenuAlias}"               -> Just MenuAlias
    "{MenuSelectAlias}"         -> Just MenuSelectAlias
    "{MenuClearKeyAlias}"       -> Just MenuClearKeyAlias
    "{MenuSlotChangeAlias}"     -> Just MenuSlotChangeAlias
    _                           -> Nothing

inputRawDataToImageFilePath :: GamepadType -> InputRawData -> Maybe PackResourceFilePath
inputRawDataToImageFilePath gamepadType inputRawData = case gamepadType of
    Xbox360GamepadType   -> xbox360ImgFilePath
    XboxOneGamepadType   -> xboxOneImgFilePath
    PS4GamepadType       -> ps4ImgFilePath
    PS5GamepadType       -> ps5ImgFilePath
    PSXGamepadType       -> psxImgFilePath
    SwitchProGamepadType -> switchProImgFilePath
    where
        uiPackPath    = \f -> PackResourceFilePath "data/ui/ui.pack" f
        xbox360Path   = \f -> Just $ uiPackPath ("controller-xbox-" ++ f)
        xboxOnePath   = \f -> Just $ uiPackPath ("controller-xbox-one-" ++ f)
        ps4Path       = \f -> Just $ uiPackPath ("controller-ps4-" ++ f)
        ps5Path       = \f -> Just $ uiPackPath ("controller-ps5-" ++ f)
        psxPath       = \f -> Just $ uiPackPath ("controller-psx-" ++ f)
        switchProPath = \f -> Just $ uiPackPath ("controller-switch-pro-" ++ f)

        xbox360ImgFilePath = case inputRawData of
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_A             -> xbox360Path "a.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_B             -> xbox360Path "b.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_X             -> xbox360Path "x.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_Y             -> xbox360Path "y.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_BACK          -> xbox360Path "back.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_START         -> xbox360Path "start.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_LEFTSTICK     -> xbox360Path "lstick.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_RIGHTSTICK    -> xbox360Path "rstick.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_LEFTSHOULDER  -> xbox360Path "lb.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_RIGHTSHOULDER -> xbox360Path "rb.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_DPAD_UP       -> xbox360Path "dpad-up.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_DPAD_DOWN     -> xbox360Path "dpad-down.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_DPAD_LEFT     -> xbox360Path "dpad-left.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_DPAD_RIGHT    -> xbox360Path "dpad-right.image"
            GamepadNegAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_LEFTX          -> xbox360Path "lstick-left.image"
            GamepadNegAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_LEFTY          -> xbox360Path "lstick-up.image"
            GamepadPosAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_LEFTX          -> xbox360Path "lstick-right.image"
            GamepadPosAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_LEFTY          -> xbox360Path "lstick-down.image"
            GamepadPosAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_TRIGGERLEFT    -> xbox360Path "lt.image"
            GamepadPosAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_TRIGGERRIGHT   -> xbox360Path "rt.image"
            _                                                                -> Nothing

        xboxOneImgFilePath = case inputRawData of
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_BACK  -> xboxOnePath "back.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_START -> xboxOnePath "start.image"
            _                                                        -> xbox360ImgFilePath

        ps4ImgFilePath = case inputRawData of
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_A             -> ps4Path "cross.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_B             -> ps4Path "circle.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_X             -> ps4Path "square.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_Y             -> ps4Path "triangle.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_BACK          -> ps4Path "share.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_START         -> ps4Path "options.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_LEFTSTICK     -> ps4Path "l3-.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_RIGHTSTICK    -> ps4Path "r3-.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_LEFTSHOULDER  -> ps4Path "l1-.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_RIGHTSHOULDER -> ps4Path "r1-.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_DPAD_UP       -> ps4Path "dpad-up.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_DPAD_DOWN     -> ps4Path "dpad-down.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_DPAD_LEFT     -> ps4Path "dpad-left.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_DPAD_RIGHT    -> ps4Path "dpad-right.image"
            GamepadNegAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_LEFTX          -> ps4Path "lstick-left.image"
            GamepadNegAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_LEFTY          -> ps4Path "lstick-up.image"
            GamepadPosAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_LEFTX          -> ps4Path "lstick-right.image"
            GamepadPosAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_LEFTY          -> ps4Path "lstick-down.image"
            GamepadPosAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_TRIGGERLEFT    -> ps4Path "l2-.image"
            GamepadPosAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_TRIGGERRIGHT   -> ps4Path "r2-.image"
            _                                                                -> Nothing

        ps5ImgFilePath = case inputRawData of
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_BACK  -> ps5Path "create.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_START -> ps5Path "options.image"
            _                                                        -> ps4ImgFilePath

        psxImgFilePath = case inputRawData of
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_BACK  -> psxPath "select.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_START -> psxPath "start.image"
            _                                                        -> ps4ImgFilePath

        switchProImgFilePath = case inputRawData of
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_A             -> switchProPath "a.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_B             -> switchProPath "b.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_X             -> switchProPath "x.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_Y             -> switchProPath "y.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_BACK          -> switchProPath "minus.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_START         -> switchProPath "plus.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_LEFTSTICK     -> switchProPath "lstick.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_RIGHTSTICK    -> switchProPath "rstick.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_LEFTSHOULDER  -> switchProPath "l.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_RIGHTSHOULDER -> switchProPath "r.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_DPAD_UP       -> switchProPath "dpad-up.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_DPAD_DOWN     -> switchProPath "dpad-down.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_DPAD_LEFT     -> switchProPath "dpad-left.image"
            GamepadButtonRawData SDL.Raw.SDL_CONTROLLER_BUTTON_DPAD_RIGHT    -> switchProPath "dpad-right.image"
            GamepadNegAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_LEFTX          -> switchProPath "lstick-left.image"
            GamepadNegAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_LEFTY          -> switchProPath "lstick-up.image"
            GamepadPosAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_LEFTX          -> switchProPath "lstick-right.image"
            GamepadPosAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_LEFTY          -> switchProPath "lstick-down.image"
            GamepadPosAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_TRIGGERLEFT    -> switchProPath "zl.image"
            GamepadPosAxisRawData SDL.Raw.SDL_CONTROLLER_AXIS_TRIGGERRIGHT   -> switchProPath "zr.image"
            _                                                                -> Nothing

mkFontTexture :: (GraphicsRead m, MonadIO m) => T.Text -> Font -> Color -> m Texture
mkFontTexture text font color =
    let
        sdlFont       = _sdlFont font
        sdlFontStyles = _sdlFontStyles font
        text'         = if T.null text then " " else text  -- SDL.Font will crash if the text is 0 width
        fakeFilePath  = show font ++  show color ++ T.unpack text'
    in do
        SDL.Font.setStyle sdlFont sdlFontStyles
        surface <- SDL.Font.blended sdlFont (colorToV4 color) text'
        loadTextureEx fakeFilePath surface

formatMouseKbInputAlias :: MonadIO m => Maybe Int -> [InputRawData] -> m T.Text
formatMouseKbInputAlias entriesIdx inputRawDatas = case filterInputRawDataMouseKb inputRawDatas of
    (rd0:rd1:_)
        | Just idx <- entriesIdx, idx == 1 -> T.pack . printf "[%s]" <$> formatInputRawData rd1
        | isNothing entriesIdx             -> do
            txt0 <- formatInputRawData rd0
            txt1 <- formatInputRawData rd1
            return $ T.pack (printf "[%s]%s[%s]" txt0 separatorText txt1)

    (rd:_)
        | maybe True (== 0) entriesIdx -> T.pack . printf "[%s]" <$> formatInputRawData rd

    _ -> return T.empty

toGamepadImageFilePath :: InputRead m => Maybe Int -> [InputRawData] -> m [PackResourceFilePath]
toGamepadImageFilePath entriesIdx inputRawDatas = do
    lastGamepadType <- _lastGamepadType <$> readInputState
    let
        inputRawDatas' = case entriesIdx of
            Nothing  -> filterInputRawDataGamepad inputRawDatas
            Just idx -> maybe [] pure (filterInputRawDataGamepad inputRawDatas !!? idx)
    return $ mapMaybe (inputRawDataToImageFilePath lastGamepadType) inputRawDatas'

loadGamepadImages
    :: forall m. (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => T.Text
    -> Font
    -> Maybe Int
    -> [InputRawData]
    -> m [(Pos2, Image)]
loadGamepadImages startTxt font entriesIdx inputRawDatas = do
    let sdlFont                = _sdlFont font
    separatorTxtWidth         <- fromIntegral . fst <$> SDL.Font.size sdlFont separatorText
    (startWidth, startHeight) <- SDL.Font.size sdlFont startTxt

    let
        loadImages :: Float -> [PackResourceFilePath] -> m [(Pos2, Image)]
        loadImages _ []                    = return []
        loadImages totalWidth (path:paths) = do
            img <- loadPackImage path
            let
                imgWidth = imageWidth img
                offsetX  = totalWidth + imgWidth / 2.0
                offsetY  = fromIntegral startHeight / 2.0

            let totalWidth' = totalWidth + imgWidth + separatorTxtWidth
            ((Pos2 offsetX offsetY, img):) <$> loadImages totalWidth' paths

    loadImages (fromIntegral startWidth) =<< toGamepadImageFilePath entriesIdx inputRawDatas

formatGamepadTextureText :: MonadIO m => T.Text -> T.Text -> Font -> [(Pos2, Image)] -> m T.Text
formatGamepadTextureText startTxt endTxt font gamepadImgs = case gamepadImgs of
    ((_, img0):(_, img1):_) -> do
        spaces0 <- imageReplacementSpacesText img0 font
        spaces1 <- imageReplacementSpacesText img1 font
        return $ startTxt <> spaces0 <> separatorText <> spaces1 <> endTxt

    ((_, img0):_) -> do
        spaces0 <- imageReplacementSpacesText img0 font
        return $ startTxt <> spaces0 <> endTxt

    _ -> return $ startTxt <> endTxt

lookupInputAliasRawDatas :: InputRead m => Maybe InputAlias -> m [InputRawData]
lookupInputAliasRawDatas = \case
    Nothing    -> return []
    Just alias -> do
        rawDataMap <- _inputAliasRawDataMap <$> readInputState
        return $ HM.lookupDefault [] alias rawDataMap

mkInputDisplayText
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => T.Text
    -> FontType
    -> Color
    -> m InputDisplayText
mkInputDisplayText txt fontType color = do
    font                         <- getGraphicsFont fontType
    (prefixSymbolImg, parsedTxt) <- parseLoadPrefixSymbolImage txt font

    let
        (startTxt, remainderTxt)  = T.breakOn "{" parsedTxt
        (specialTxt, endTxt)      = T.breakOnEnd "}" remainderTxt
        (specialTxt', entriesIdx) = case T.splitOn "." specialTxt of
            [spTxtPre, "0}"] -> (spTxtPre <> "}", Just 0)
            [spTxtPre, "1}"] -> (spTxtPre <> "}", Just 1)
            _                -> (specialTxt, Nothing)
        inputAlias                  = specialTextToInputAlias specialTxt'

    inputRawDatas        <- lookupInputAliasRawDatas inputAlias
    mouseKbInputAliasTxt <- formatMouseKbInputAlias entriesIdx inputRawDatas
    mouseKbTexture       <- mkFontTexture (startTxt <> mouseKbInputAliasTxt <> endTxt) font color
    gamepadImgs          <- loadGamepadImages startTxt font entriesIdx inputRawDatas
    gamepadTxt           <- formatGamepadTextureText startTxt endTxt font gamepadImgs
    gamepadTexture       <- mkFontTexture gamepadTxt font color
    gamepadType          <- _lastGamepadType <$> readInputState

    return $ InputDisplayText
        { _inputAlias        = inputAlias
        , _inputRawDatas     = inputRawDatas
        , _text              = txt
        , _font              = font
        , _color             = color
        , _mouseKbTexture    = mouseKbTexture
        , _gamepadTexture    = gamepadTexture
        , _gamepadImages     = gamepadImgs
        , _gamepadType       = gamepadType
        , _prefixSymbolImage = prefixSymbolImg
        }

updateInputDisplayText
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => InputDisplayText
    -> m InputDisplayText
updateInputDisplayText inputDisplayTxt = do
    inputRawDatas <- lookupInputAliasRawDatas $ _inputAlias inputDisplayTxt
    gamepadType   <- _lastGamepadType <$> readInputState
    let isNoChange = _inputRawDatas inputDisplayTxt == inputRawDatas && _gamepadType inputDisplayTxt == gamepadType

    if
        | isNoChange -> return inputDisplayTxt
        | otherwise  ->
            let
                txt      = _text (inputDisplayTxt :: InputDisplayText)
                fontType = _type (_font inputDisplayTxt :: Font)
                color    = _color inputDisplayTxt
            in mkInputDisplayText txt fontType color

drawInputDisplayText :: (GraphicsReadWrite m, InputRead m, MonadIO m) => Pos2 -> ZIndex -> InputDisplayText -> m ()
drawInputDisplayText pos zIndex inputDisplayTxt = (_lastUsedInputType <$> readInputState) >>= \case
    GamepadInputType -> drawInputDisplayTextGamepad pos zIndex inputDisplayTxt
    MouseKbInputType -> drawInputDisplayTextMouseKb pos zIndex inputDisplayTxt

drawPrefixSymbolImage :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> ZIndex -> InputDisplayText -> m ()
drawPrefixSymbolImage pos zIndex inputDisplayTxt = case _prefixSymbolImage inputDisplayTxt of
    Nothing  -> return ()
    Just img -> do
        let
            sdlFont = _sdlFont $ _font inputDisplayTxt
            txt     = _text (inputDisplayTxt :: InputDisplayText)
        txtHeight <- fromIntegral . snd <$> SDL.Font.size sdlFont txt
        let
            offset = Pos2 (imageWidth img / 2.0) (txtHeight / 2.0)
            pos'   = vecRoundXY $ pos `vecAdd` offset
        drawImage pos' RightDir zIndex img

drawInputDisplayTextMouseKb :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> ZIndex -> InputDisplayText -> m ()
drawInputDisplayTextMouseKb pos zIndex inputDisplayTxt = do
    drawTexture zeroPos2 (vecRoundXY pos) RightDir zIndex (_mouseKbTexture inputDisplayTxt)
    drawPrefixSymbolImage pos zIndex inputDisplayTxt

drawInputDisplayTextGamepad :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> ZIndex -> InputDisplayText -> m ()
drawInputDisplayTextGamepad pos zIndex inputDisplayTxt = do
    drawTexture zeroPos2 (vecRoundXY pos) RightDir zIndex (_gamepadTexture inputDisplayTxt)
    for_ (_gamepadImages inputDisplayTxt) $ \(offset, img) ->
        drawImage (vecRoundXY $ pos `vecAdd` offset) RightDir zIndex img
    drawPrefixSymbolImage pos zIndex inputDisplayTxt

drawInputDisplayTextCentered
    :: (GraphicsReadWrite m, InputRead m, MonadIO m)
    => Pos2
    -> ZIndex
    -> InputDisplayText
    -> m ()
drawInputDisplayTextCentered pos zIndex inputDisplayTxt = (_lastUsedInputType <$> readInputState) >>= \case
    GamepadInputType -> drawInputDisplayTextGamepadCentered pos zIndex inputDisplayTxt
    MouseKbInputType -> drawInputDisplayTextMouseKbCentered pos zIndex inputDisplayTxt

drawInputDisplayTextMouseKbCentered :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> ZIndex -> InputDisplayText -> m ()
drawInputDisplayTextMouseKbCentered (Pos2 x y) zIndex inputDisplayTxt =
    let
        texture = _mouseKbTexture inputDisplayTxt
        width   = fromIntegral $ _width texture
        height  = fromIntegral $ _height texture
        pos     = Pos2 (x - width / 2.0) (y - height / 2.0)
    in drawInputDisplayTextMouseKb pos zIndex inputDisplayTxt

drawInputDisplayTextGamepadCentered :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> ZIndex -> InputDisplayText -> m ()
drawInputDisplayTextGamepadCentered (Pos2 x y) zIndex inputDisplayTxt =
    let
        texture = _gamepadTexture inputDisplayTxt
        width   = fromIntegral $ _width texture
        height  = fromIntegral $ _height texture
        pos     = Pos2 (x - width / 2.0) (y - height / 2.0)
    in drawInputDisplayTextGamepad pos zIndex inputDisplayTxt

inputDisplayTextWidth :: InputRead m => InputDisplayText -> m Float
inputDisplayTextWidth inputDisplayTxt = (_lastUsedInputType <$> readInputState) <&> \case
    GamepadInputType -> fromIntegral $ _width (_gamepadTexture inputDisplayTxt)
    MouseKbInputType -> fromIntegral $ _width (_mouseKbTexture inputDisplayTxt)

inputDisplayTextHeight :: InputRead m => InputDisplayText -> m Float
inputDisplayTextHeight inputDisplayTxt = (_lastUsedInputType <$> readInputState) <&> \case
    GamepadInputType -> fromIntegral $ _height (_gamepadTexture inputDisplayTxt)
    MouseKbInputType -> fromIntegral $ _height (_mouseKbTexture inputDisplayTxt)
