module Configs.All.PlayerSkill.MarkRecall
    ( MarkRecallConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Util

data MarkRecallConfig = MarkRecallConfig
    { _markerCooldown :: Secs
    , _markerOffset   :: Pos2
    , _particleOffset :: Pos2
    , _recallOffset   :: Pos2
    , _recallHitlag   :: Secs
    }
    deriving Generic

instance FromJSON MarkRecallConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
