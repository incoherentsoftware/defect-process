module Player.SecondarySkill.Manager.Util
    ( setSecondarySkillManagerOrder
    ) where

import Control.Monad.State (execState, modify)

import Player.SecondarySkill.Types
import Player.SecondarySkill.Manager.Types
import Util

setSecondarySkillManagerOrder
    :: Maybe SecondarySkillType
    -> Maybe SecondarySkillType
    -> Maybe SecondarySkillType
    -> SecondarySkillManager
    -> SecondarySkillManager
setSecondarySkillManagerOrder neutralSlotType upSlotType downSlotType secondarySkillMgr =
    flip execState secondarySkillMgr $ do
        modify $ \ssm -> case neutralSlotType of
            Just neutralType
                | Just (Some ss) <- _upSlot ssm, _type ss == neutralType -> ssm
                    { _neutralSlot = _upSlot ssm
                    , _upSlot      = _neutralSlot ssm
                    }

                | Just (Some ss) <- _downSlot ssm, _type ss == neutralType -> ssm
                    { _neutralSlot = _downSlot ssm
                    , _downSlot    = _neutralSlot ssm
                    }

            _ -> ssm

        modify $ \ssm -> case upSlotType of
            Just upType
                | Just (Some ss) <- _neutralSlot ssm, _type ss == upType -> ssm
                    { _upSlot      = _neutralSlot ssm
                    , _neutralSlot = _upSlot ssm
                    }

                | Just (Some ss) <- _downSlot ssm, _type ss == upType -> ssm
                    { _upSlot   = _downSlot ssm
                    , _downSlot = _upSlot ssm
                    }

            _ -> ssm

        modify $ \ssm -> case downSlotType of
            Just downType
                | Just (Some ss) <- _neutralSlot ssm, _type ss == downType -> ssm
                    { _downSlot    = _neutralSlot ssm
                    , _neutralSlot = _downSlot ssm
                    }

                | Just (Some ss) <- _upSlot ssm, _type ss == downType -> ssm
                    { _downSlot = _upSlot ssm
                    , _upSlot   = _downSlot ssm
                    }

            _ -> ssm
