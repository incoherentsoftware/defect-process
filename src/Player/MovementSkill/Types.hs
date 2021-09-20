module Player.MovementSkill.Types
    ( MovementSkillType(..)
    , MovementSkillUpdate
    , MovementSkillThink
    , MovementSkillUpdateDynamic
    , MovementSkillDraw
    , MovementSkill(..)
    ) where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Dynamic     (Dynamic)
import GHC.Generics     (Generic)
import qualified Data.Text as T

import AppEnv.Types
import Msg.Types
import Player.MovementSkill.Status
import Util
import Window.Graphics
import {-# SOURCE #-} Player.Types

data MovementSkillType
    = DashSkill
    | TeleportSkill
    | GrappleSkill
    deriving (Bounded, Enum, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance PrettyShow MovementSkillType where
    prettyShow :: MovementSkillType -> T.Text
    prettyShow = \case
        DashSkill     -> "Dash"
        TeleportSkill -> "Teleport"
        GrappleSkill  -> "Grapple Hook"

type MovementSkillUpdate d m      = Player -> MovementSkill d -> m (MovementSkill d)
type MovementSkillThink d m       = Bool -> Player -> MovementSkill d -> m [Msg ThinkPlayerMsgsPhase]
type MovementSkillUpdateDynamic d = Dynamic -> MovementSkill d -> MovementSkill d
type MovementSkillDraw d m        = Player -> MovementSkill d -> m ()

data MovementSkill d = MovementSkill
    { _data             :: d
    , _type             :: MovementSkillType
    , _status           :: MovementSkillStatus
    , _sprite           :: Maybe Sprite
    , _cooldown         :: Secs
    , _numCharges       :: Int
    , _canRefreshCharge :: Bool
    , _think            :: MovementSkillThink d (AppEnv ThinkPlayerMsgsPhase)
    , _update           :: MovementSkillUpdate d (AppEnv UpdatePlayerMsgsPhase)
    , _updateDynamic    :: MovementSkillUpdateDynamic d
    , _draw             :: MovementSkillDraw d (AppEnv DrawMsgsPhase)
    }
