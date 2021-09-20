module Window.InputState.RawData
    ( InputRawData(..)
    , filterInputRawDataMouseKb
    , filterInputRawDataGamepad
    , formatInputRawData
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.String       (peekCString)
import qualified Data.Text as T

import Data.Aeson.Types ((.:), (.=), FromJSON, Parser, ToJSON, Value(Object), object, parseJSON, toJSON, typeMismatch)
import Data.Hashable    (Hashable)
import GHC.Generics     (Generic)
import GHC.Int          (Int32)
import GHC.Word         (Word32, Word8)
import qualified SDL
import qualified SDL.Input.GameController as SDL
import qualified SDL.Internal.Numbered as SDL.Internal
import qualified SDL.Raw

returnKeyName   = "Return" :: String
enterKeyNameTxt = "Enter"  :: T.Text

data InputRawData
    = KeyRawData Word32
    | MouseButtonRawData Word8
    | MouseWheelScrollXRawData Int32
    | MouseWheelScrollYRawData Int32
    | GamepadButtonRawData Int32
    | GamepadNegAxisRawData Int32
    | GamepadPosAxisRawData Int32
    deriving (Eq, Generic, Hashable, Ord)

instance FromJSON InputRawData where
    parseJSON :: Value -> Parser InputRawData
    parseJSON (Object v) = v .: "type" >>= \case
        "KeyRawData"               -> KeyRawData <$> v .: "value"
        "MouseButtonRawData"       -> MouseButtonRawData <$> v .: "value"
        "MouseWheelScrollXRawData" -> MouseWheelScrollXRawData <$> v .: "value"
        "MouseWheelScrollYRawData" -> MouseWheelScrollYRawData <$> v .: "value"
        "GamepadButtonRawData"     -> GamepadButtonRawData <$> v .: "value"
        "GamepadNegAxisRawData"    -> GamepadNegAxisRawData <$> v .: "value"
        "GamepadPosAxisRawData"    -> GamepadPosAxisRawData <$> v .: "value"
        value                      -> typeMismatch "InputRawData" value
    parseJSON value      = typeMismatch "InputRawData" value

instance ToJSON InputRawData where
    toJSON :: InputRawData -> Value
    toJSON = \case
        KeyRawData value               -> object' "KeyRawData" value
        MouseButtonRawData value       -> object' "MouseButtonRawData" value
        MouseWheelScrollXRawData value -> object' "MouseWheelScrollXRawData" value
        MouseWheelScrollYRawData value -> object' "MouseWheelScrollYRawData" value
        GamepadButtonRawData value     -> object' "GamepadButtonRawData" value
        GamepadNegAxisRawData value    -> object' "GamepadNegAxisRawData" value
        GamepadPosAxisRawData value    -> object' "GamepadPosAxisRawData" value
        where
            object' :: ToJSON a => T.Text -> a -> Value
            object' t v = object ["type" .= t, "value" .= v]

isMouseKb :: InputRawData -> Bool
isMouseKb = \case
    KeyRawData _               -> True
    MouseButtonRawData _       -> True
    MouseWheelScrollXRawData _ -> True
    MouseWheelScrollYRawData _ -> True
    GamepadButtonRawData _     -> False
    GamepadNegAxisRawData _    -> False
    GamepadPosAxisRawData _    -> False

filterInputRawDataMouseKb :: [InputRawData] -> [InputRawData]
filterInputRawDataMouseKb rawDatas = filter isMouseKb rawDatas

filterInputRawDataGamepad :: [InputRawData] -> [InputRawData]
filterInputRawDataGamepad rawDatas = filter (not . isMouseKb) rawDatas

formatInputRawData :: MonadIO m => InputRawData -> m T.Text
formatInputRawData aliasRawData = case aliasRawData of
    KeyRawData val -> do
        keyName  <- SDL.Raw.getKeyName =<< SDL.Raw.getKeyFromScancode val
        keyName' <- liftIO $ peekCString keyName

        return $ if
            | keyName' == returnKeyName -> enterKeyNameTxt
            | otherwise                 -> T.pack keyName'

    MouseButtonRawData val -> return . T.pack $ case SDL.Internal.fromNumber val of
        SDL.ButtonLeft   -> "Mouse Left"
        SDL.ButtonMiddle -> "Mouse Middle"
        SDL.ButtonRight  -> "Mouse Right"
        SDL.ButtonX1     -> "Mouse 4"
        SDL.ButtonX2     -> "Mouse 5"
        _                -> "???"

    MouseWheelScrollXRawData val -> return . T.pack $ if
        | val < 0   -> "Mouse Scroll Left"
        | val > 0   -> "Mouse Scroll Right"
        | otherwise -> "???"

    MouseWheelScrollYRawData val -> return . T.pack $ if
        | val < 0   -> "Mouse Scroll Down"
        | val > 0   -> "Mouse Scroll Up"
        | otherwise -> "???"

    GamepadButtonRawData val -> return $ case SDL.Internal.fromNumber val of
        SDL.ControllerButtonLeftStick     -> "ControllerButtonLS"
        SDL.ControllerButtonRightStick    -> "ControllerButtonRS"
        SDL.ControllerButtonLeftShoulder  -> "ControllerButtonLB"
        SDL.ControllerButtonRightShoulder -> "ControllerButtonRB"
        SDL.ControllerButtonGuide         -> "ControllerButtonGuide"
        controllerBtn                     -> T.pack $ show controllerBtn

    GamepadNegAxisRawData val -> return . T.pack $ gamepadAxis val ++ "-"
    GamepadPosAxisRawData val -> return . T.pack $ gamepadAxis val ++ "+"

    where
        gamepadAxis = \val -> if
            | val == SDL.Raw.SDL_CONTROLLER_AXIS_LEFTX        -> "Gamepad LAxis X"
            | val == SDL.Raw.SDL_CONTROLLER_AXIS_LEFTY        -> "Gamepad LAxis Y"
            | val == SDL.Raw.SDL_CONTROLLER_AXIS_RIGHTX       -> "Gamepad RAxis X"
            | val == SDL.Raw.SDL_CONTROLLER_AXIS_RIGHTY       -> "Gamepad RAxis Y"
            | val == SDL.Raw.SDL_CONTROLLER_AXIS_TRIGGERLEFT  -> "Gamepad LTrigger"
            | val == SDL.Raw.SDL_CONTROLLER_AXIS_TRIGGERRIGHT -> "Gamepad RTrigger"
            | otherwise                                       -> "???"
