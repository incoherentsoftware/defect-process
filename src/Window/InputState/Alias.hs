module Window.InputState.Alias
    ( InputAlias(..)
    , InputAliasData(..)
    , InputAliasRawDataMap
    , isInputAliasRebindable
    , filterInputAliasRawDataMapRebindable
    , toInputAliasData
    ) where

import Data.Aeson.Types (FromJSON, FromJSONKey(fromJSONKey), ToJSON, ToJSONKey(toJSONKey))
import Data.Aeson.Types (defaultOptions, defaultJSONKeyOptions)
import Data.Aeson.Types (genericFromJSONKey, genericParseJSON, genericToJSON, genericToJSONKey, parseJSON, toJSON)
import Data.Hashable    (Hashable)
import Data.Maybe       (fromMaybe)
import GHC.Generics     (Generic)
import qualified Data.HashMap.Lazy as HM
import qualified SDL
import qualified SDL.Input.GameController as SDL
import qualified SDL.Internal.Numbered as SDL.Internal
import qualified SDL.Raw

import Util
import Window.InputState.RawData

data InputAlias
    = LeftAlias
    | RightAlias
    | DownAlias
    | UpAlias
    | JumpAlias
    | WeaponAlias
    | ShootAlias
    | SwitchWeaponAlias
    | SwitchGunAlias
    | InteractAlias
    | MovementSkillAlias
    | SecondarySkillAlias
    | LockOnCursorAlias
    | LockOnClearAlias
    | LockOnSwitchTargetAlias
    | MenuAlias
    | MenuBackAlias
    | MenuTabLeftAlias
    | MenuTabRightAlias
    | MenuQuitHotkeyAlias
    | MenuLeftAlias
    | MenuRightAlias
    | MenuDownAlias
    | MenuUpAlias
    | MenuSelectAlias
    | MenuClearKeyAlias
    | MenuSlotChangeAlias
    | DevConsoleAlias
    deriving (Eq, Generic, Hashable, Show)
    deriving anyclass PrettyShow

instance FromJSON InputAlias where
    parseJSON value = genericParseJSON defaultOptions value

instance FromJSONKey InputAlias where
    fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

instance ToJSON InputAlias where
    toJSON = genericToJSON defaultOptions

instance ToJSONKey InputAlias where
    toJSONKey = genericToJSONKey defaultJSONKeyOptions

data InputAliasData = InputAliasData
    { _keys               :: [SDL.Scancode]
    , _mouseButtons       :: [SDL.MouseButton]
    , _mouseWheelScrollXs :: [Int]
    , _mouseWheelScrollYs :: [Int]
    , _gamepadButtons     :: [SDL.ControllerButton]
    , _negAxes            :: [SDL.Raw.GameControllerAxis]
    , _posAxes            :: [SDL.Raw.GameControllerAxis]
    }

type InputAliasRawDataMap = HM.HashMap InputAlias [InputRawData]

isInputAliasRebindable :: InputAlias -> Bool
isInputAliasRebindable = \case
    LeftAlias               -> True
    RightAlias              -> True
    DownAlias               -> True
    UpAlias                 -> True
    JumpAlias               -> True
    WeaponAlias             -> True
    ShootAlias              -> True
    SwitchWeaponAlias       -> True
    SwitchGunAlias          -> True
    InteractAlias           -> True
    MovementSkillAlias      -> True
    SecondarySkillAlias     -> True
    LockOnCursorAlias       -> True
    LockOnClearAlias        -> True
    LockOnSwitchTargetAlias -> True
    _                       -> False

filterInputAliasRawDataMapRebindable :: InputAliasRawDataMap -> InputAliasRawDataMap
filterInputAliasRawDataMapRebindable inputAliasRawDataMap =
    HM.filterWithKey (\inputAlias _ -> isInputAliasRebindable inputAlias) inputAliasRawDataMap

toInputAliasData :: InputAlias -> InputAliasRawDataMap -> InputAliasData
toInputAliasData alias aliasRawDataMap = InputAliasData
    { _keys               = [SDL.Scancode v | KeyRawData v <- aliasRawDatas]
    , _mouseButtons       = [SDL.Internal.fromNumber v | MouseButtonRawData v <- aliasRawDatas]
    , _mouseWheelScrollXs = [fromIntegral v | MouseWheelScrollXRawData v <- aliasRawDatas]
    , _mouseWheelScrollYs = [fromIntegral v | MouseWheelScrollYRawData v <- aliasRawDatas]
    , _gamepadButtons     = [SDL.Internal.fromNumber v | GamepadButtonRawData v <- aliasRawDatas]
    , _negAxes            = [v | GamepadNegAxisRawData v <- aliasRawDatas]
    , _posAxes            = [v | GamepadPosAxisRawData v <- aliasRawDatas]
    }
    where aliasRawDatas = fromMaybe [] (HM.lookup alias aliasRawDataMap)
