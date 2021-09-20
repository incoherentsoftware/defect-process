module Configs.All.Settings.Controls
    ( ControlsConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)
import qualified Data.Text as T

import Util
import Window.InputState.Alias

data ControlsConfig = ControlsConfig
    { _xboxOneNameSubstrings   :: [T.Text]
    , _ps5NameSubstrings       :: [T.Text]
    , _ps4NameSubstrings       :: [T.Text]
    , _psxNameSubstrings       :: [T.Text]
    , _switchProNameSubstrings :: [T.Text]
    , _inputAliasRawDataMap    :: InputAliasRawDataMap
    }
    deriving Generic

instance FromJSON ControlsConfig where
    parseJSON value = genericParseJSON aesonFieldDropUnderscore value
