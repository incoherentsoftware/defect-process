module Configs.All.Progress
    ( ProgressConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)
import qualified Data.Set as S

import Util
import World.Audio.LayeredMusic.Types
import World.Util
import {-# SOURCE #-} Player.Gun.Types
import {-# SOURCE #-} Player.MovementSkill.Types
import {-# SOURCE #-} Player.SecondarySkill.Types
import {-# SOURCE #-} Player.Weapon.Types

data ProgressConfig = ProgressConfig
    { _totalGold               :: GoldValue
    , _unlockedWeapons         :: S.Set WeaponType
    , _unlockedGuns            :: S.Set GunType
    , _unlockedMovementSkills  :: S.Set MovementSkillType
    , _unlockedSecondarySkills :: S.Set SecondarySkillType
    , _unlockedMusic           :: S.Set LayeredMusicType
    }
    deriving Generic

instance FromJSON ProgressConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
