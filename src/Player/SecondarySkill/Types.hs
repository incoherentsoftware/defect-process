module Player.SecondarySkill.Types
    ( SecondarySkillSlot(..)
    , SecondarySkillType(..)
    , SecondarySkillUpdate
    , SecondarySkillThink
    , SecondarySkillUpdateDynamic
    , SecondarySkillDraw
    , SecondarySkillOnCooldown
    , SecondarySkill(..)
    ) where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Dynamic     (Dynamic)
import GHC.Generics     (Generic)
import qualified Data.Text as T

import AppEnv.Types
import Msg.Types
import Util
import {-# SOURCE #-} Player.Types

data SecondarySkillSlot
    = SecondarySkillNeutralSlot
    | SecondarySkillUpSlot
    | SecondarySkillDownSlot

data SecondarySkillType
    = StoneFormSkill
    | FlightSkill
    | FastFallSkill
    deriving (Bounded, Enum, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance PrettyShow SecondarySkillType where
    prettyShow :: SecondarySkillType -> T.Text
    prettyShow = \case
        StoneFormSkill -> "Stone Form"
        FlightSkill    -> "Flight"
        FastFallSkill  -> "Fast Fall"

type SecondarySkillUpdate d m      = Player -> SecondarySkill d -> m (SecondarySkill d)
type SecondarySkillThink d m       =
    Bool -> Player -> SecondarySkillSlot -> SecondarySkill d -> m [Msg ThinkPlayerMsgsPhase]
type SecondarySkillUpdateDynamic d = Dynamic -> SecondarySkill d -> SecondarySkill d
type SecondarySkillDraw d m        = Player -> SecondarySkill d -> m ()
type SecondarySkillOnCooldown d    = SecondarySkill d -> Bool

data SecondarySkill d = SecondarySkill
    { _data          :: d
    , _type          :: SecondarySkillType
    , _think         :: SecondarySkillThink d (AppEnv ThinkPlayerMsgsPhase)
    , _update        :: SecondarySkillUpdate d (AppEnv UpdatePlayerMsgsPhase)
    , _updateDynamic :: SecondarySkillUpdateDynamic d
    , _draw          :: SecondarySkillDraw d (AppEnv DrawMsgsPhase)
    , _onCooldown    :: SecondarySkillOnCooldown d
    }
