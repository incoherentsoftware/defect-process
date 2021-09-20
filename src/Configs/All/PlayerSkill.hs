module Configs.All.PlayerSkill
    ( PlayerSkillConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Configs.All.PlayerSkill.Dash
import Configs.All.PlayerSkill.Grapple
import Configs.All.PlayerSkill.StoneForm
import Configs.All.PlayerSkill.Teleport
import Util

data PlayerSkillConfig = PlayerSkillConfig
    { _dash      :: DashConfig
    , _teleport  :: TeleportConfig
    , _grapple   :: GrappleConfig
    , _stoneForm :: StoneFormConfig
    }
    deriving Generic

instance FromJSON PlayerSkillConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
