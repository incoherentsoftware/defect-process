module Player.SecondarySkill.Manager
    ( module Player.SecondarySkill.Manager.Types
    , mkSecondarySkillManager
    , thinkSecondarySkillManager
    , updateSecondarySkillManager
    , giveSecondarySkillManagerSkill
    , clearSecondarySkillManagerSkill
    , secondarySkillManagerNeutralSlotType
    , secondarySkillManagerUpSlotType
    , secondarySkillManagerDownSlotType
    ) where

import Data.Maybe (isNothing)

import AppEnv
import Msg
import Player.SecondarySkill
import Player.SecondarySkill.Manager.Types
import Player.Types
import Util

mkSecondarySkillManager :: SecondarySkillManager
mkSecondarySkillManager = SecondarySkillManager
    { _neutralSlot = Nothing
    , _upSlot      = Nothing
    , _downSlot    = Nothing
    }

thinkSecondarySkillManager
    :: Bool
    -> Player
    -> SecondarySkillManager
    -> AppEnv ThinkPlayerMsgsPhase [Msg ThinkPlayerMsgsPhase]
thinkSecondarySkillManager canUseSkill player secondarySkillMgr = concat <$> (sequenceA $
    [ maybe (return []) (think SecondarySkillNeutralSlot) (_neutralSlot secondarySkillMgr)
    , maybe (return []) (think SecondarySkillUpSlot) (_upSlot secondarySkillMgr)
    , maybe (return []) (think SecondarySkillDownSlot) (_downSlot secondarySkillMgr)
    ])
    where
        think :: SecondarySkillSlot -> Some SecondarySkill -> AppEnv ThinkPlayerMsgsPhase [Msg ThinkPlayerMsgsPhase]
        think slot (Some secondarySkill) = (_think secondarySkill) canUseSkill player slot secondarySkill

updateSecondarySkillManager :: Player -> SecondarySkillManager -> AppEnv UpdatePlayerMsgsPhase SecondarySkillManager
updateSecondarySkillManager player secondarySkillMgr =
    let
        updateSkill = \case
            Nothing        -> return Nothing
            Just (Some ss) -> Just . Some <$> updateSecondarySkill player ss
    in do
        neutralSlot <- updateSkill $ _neutralSlot secondarySkillMgr
        upSlot      <- updateSkill $ _upSlot secondarySkillMgr
        downSlot    <- updateSkill $ _downSlot secondarySkillMgr

        return $ secondarySkillMgr
            { _neutralSlot = neutralSlot
            , _upSlot      = upSlot
            , _downSlot    = downSlot
            }

giveSecondarySkillManagerSkill
    :: Maybe SecondarySkillSlot
    -> Some SecondarySkill
    -> SecondarySkillManager
    -> SecondarySkillManager
giveSecondarySkillManagerSkill slot (Some secondarySkill) secondarySkillMgr = case slot' of
    SecondarySkillNeutralSlot
        | isSameSkillType _upSlot   -> secondarySkillMgr
            { _upSlot      = _neutralSlot secondarySkillMgr
            , _neutralSlot = Just $ Some secondarySkill
            }
        | isSameSkillType _downSlot -> secondarySkillMgr
            { _downSlot    = _neutralSlot secondarySkillMgr
            , _neutralSlot = Just $ Some secondarySkill
            }
        | otherwise                 -> secondarySkillMgr {_neutralSlot = Just $ Some secondarySkill}

    SecondarySkillUpSlot
        | isSameSkillType _neutralSlot -> secondarySkillMgr
            { _neutralSlot = _upSlot secondarySkillMgr
            , _upSlot      = Just $ Some secondarySkill
            }
        | isSameSkillType _downSlot    -> secondarySkillMgr
            { _downSlot = _upSlot secondarySkillMgr
            , _upSlot   = Just $ Some secondarySkill
            }
        | otherwise                    -> secondarySkillMgr {_upSlot = Just $ Some secondarySkill}

    SecondarySkillDownSlot
        | isSameSkillType _neutralSlot -> secondarySkillMgr
            { _neutralSlot = _downSlot secondarySkillMgr
            , _downSlot    = Just $ Some secondarySkill
            }
        | isSameSkillType _upSlot      -> secondarySkillMgr
            { _upSlot   = _downSlot secondarySkillMgr
            , _downSlot = Just $ Some secondarySkill
            }
        | otherwise                    -> secondarySkillMgr {_downSlot = Just $ Some secondarySkill}

    where
        isSameSkillType = \slotF ->
            maybe False (\(Some ss) -> _type ss == _type secondarySkill) (slotF secondarySkillMgr)

        slot' = case slot of
            Just slt                                         -> slt
            Nothing
                | isSameSkillType _neutralSlot               -> SecondarySkillNeutralSlot
                | isSameSkillType _upSlot                    -> SecondarySkillUpSlot
                | isSameSkillType _downSlot                  -> SecondarySkillDownSlot
                | isNothing (_neutralSlot secondarySkillMgr) -> SecondarySkillNeutralSlot
                | isNothing (_upSlot secondarySkillMgr)      -> SecondarySkillUpSlot
                | isNothing (_downSlot secondarySkillMgr)    -> SecondarySkillDownSlot
                | otherwise                                  -> SecondarySkillNeutralSlot

clearSecondarySkillManagerSkill :: SecondarySkillType -> SecondarySkillManager -> SecondarySkillManager
clearSecondarySkillManagerSkill secondarySkillType secondarySkillMgr = secondarySkillMgr
    { _neutralSlot = clearSlot _neutralSlot
    , _upSlot      = clearSlot _upSlot
    , _downSlot    = clearSlot _downSlot
    }
    where
        clearSlot = \slotF -> case slotF secondarySkillMgr of
            Just (Some ss)
                | _type ss == secondarySkillType -> Nothing
            slot                                 -> slot

secondarySkillManagerNeutralSlotType :: SecondarySkillManager -> Maybe SecondarySkillType
secondarySkillManagerNeutralSlotType secondarySkillMgr = case _neutralSlot secondarySkillMgr of
    Nothing        -> Nothing
    Just (Some ss) -> Just $ _type ss

secondarySkillManagerUpSlotType :: SecondarySkillManager -> Maybe SecondarySkillType
secondarySkillManagerUpSlotType secondarySkillMgr = case _upSlot secondarySkillMgr of
    Nothing        -> Nothing
    Just (Some ss) -> Just $ _type ss

secondarySkillManagerDownSlotType :: SecondarySkillManager -> Maybe SecondarySkillType
secondarySkillManagerDownSlotType secondarySkillMgr = case _downSlot secondarySkillMgr of
    Nothing        -> Nothing
    Just (Some ss) -> Just $ _type ss
