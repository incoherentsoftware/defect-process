module Configs.All.PlayerSkill.FastFall
    ( FastFallConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util

data FastFallConfig = FastFallConfig
    { _fastFallStrongVelY           :: VelY
    , _fastFallMediumVelY           :: VelY
    , _fastFallSoftVelY             :: VelY
    , _fastFallStrongGroundDistance :: Distance
    , _fastFallMediumGroundDistance :: Distance
    , _fastFallSoftGroundDistance   :: Distance
    }
    deriving Generic

instance FromJSON FastFallConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
