module Configs.All.Settings.Render
    ( RenderConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Window.Graphics.Util
import Util

data RenderConfig = RenderConfig
    { _winWidth        :: Int
    , _winHeight       :: Int
    , _winMode         :: WindowMode
    , _winDisplayIndex :: Int
    , _hintOpenGL      :: Bool
    , _vsync           :: Bool
    , _manualPerformGC :: Bool
    }
    deriving Generic

instance FromJSON RenderConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
