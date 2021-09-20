module Configs.All.Settings.Input
    ( InputConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util

data InputConfig = InputConfig
    { _bufferSecs    :: Secs
    , _tapBufferSecs :: Secs
    }
    deriving Generic

instance FromJSON InputConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
