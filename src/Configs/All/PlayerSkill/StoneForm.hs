module Configs.All.PlayerSkill.StoneForm
    ( StoneFormConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Player.Meter
import Util

data StoneFormConfig = StoneFormConfig
    { _stoneFormCooldown        :: Secs
    , _stoneFormExtendMeterCost :: MeterValue
    , _meleeDeflectMeterGain    :: MeterValue
    , _rangedDeflectMeterGain   :: MeterValue
    }
    deriving Generic

instance FromJSON StoneFormConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
