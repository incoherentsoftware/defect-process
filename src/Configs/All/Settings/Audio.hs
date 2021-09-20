module Configs.All.Settings.Audio
    ( AudioConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Audio.Volume
import Util
import World.Audio.LayeredMusic.Types

data AudioConfig = AudioConfig
    { _soundsEnabled                   :: Bool
    , _musicEnabled                    :: Bool
    , _soundVolume                     :: Volume
    , _musicVolume                     :: Volume
    , _battleMusicInitialVolumePercent :: Float
    , _explorationMusic                :: LayeredMusicType
    , _battleMusic                     :: LayeredMusicType
    }
    deriving Generic

instance FromJSON AudioConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
