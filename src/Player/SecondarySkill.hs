module Player.SecondarySkill
    ( module Player.SecondarySkill.Types
    , isSecondarySkillTypeInAirOnly
    , mkSecondarySkill
    , updateSecondarySkill
    , isSecondarySkillPressed
    , isSecondarySkillPressedBuffer
    ) where

import Data.Dynamic  (fromDynamic)
import Data.Typeable (Typeable)

import AppEnv
import Msg.Phase
import Player.BufferedInputState
import Player.SecondarySkill.Types
import Player.Types
import Player.Util
import Window.InputState

isSecondarySkillTypeInAirOnly :: SecondarySkillType -> Bool
isSecondarySkillTypeInAirOnly = \case
    StoneFormSkill -> False
    FlightSkill    -> True
    FastFallSkill  -> True

mkSecondarySkill :: Typeable d => d -> SecondarySkillType -> SecondarySkill d
mkSecondarySkill dat secondarySkillType = SecondarySkill
    { _data          = dat
    , _type          = secondarySkillType
    , _think         = \_ _ _ _ -> return []
    , _update        = const $ return . id
    , _updateDynamic = updateDynamic
    , _draw          = \_ _ -> return ()
    , _onCooldown    = const False
    }

updateDynamic :: Typeable d => SecondarySkillUpdateDynamic d
updateDynamic dyn secondarySkill = case fromDynamic dyn of
    Just update -> update secondarySkill
    Nothing     -> secondarySkill

updateSecondarySkill :: Player -> SecondarySkill d -> AppEnv UpdatePlayerMsgsPhase (SecondarySkill d)
updateSecondarySkill player secondarySkill = (_update secondarySkill) player secondarySkill

isSecondarySkillPressed :: InputState -> SecondarySkillSlot -> Bool
isSecondarySkillPressed inputState slot = case slot of
    SecondarySkillNeutralSlot -> not upAliasHold && not downAliasHold && secondarySkillAliasPressed
    SecondarySkillUpSlot      -> upAliasHold && secondarySkillAliasPressed
    SecondarySkillDownSlot    -> downAliasHold && secondarySkillAliasPressed
    where
        upAliasHold                = UpAlias `aliasHold` inputState
        downAliasHold              = DownAlias `aliasHold` inputState
        secondarySkillAliasPressed = SecondarySkillAlias `aliasPressed` inputState

isSecondarySkillPressedBuffer :: Player -> SecondarySkillSlot -> Bool
isSecondarySkillPressedBuffer player slot = case slot of
    SecondarySkillNeutralSlot -> SecondarySkillNeutralInput `inPlayerInputBuffer` player
    SecondarySkillUpSlot      -> SecondarySkillUpInput `inPlayerInputBuffer` player
    SecondarySkillDownSlot    -> SecondarySkillDownInput `inPlayerInputBuffer` player
