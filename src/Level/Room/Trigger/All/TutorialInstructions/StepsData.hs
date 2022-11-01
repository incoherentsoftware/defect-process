module Level.Room.Trigger.All.TutorialInstructions.StepsData
    ( InteractAliasStatus(..)
    , StepsData(..)
    ) where

import qualified Data.Set as S

import Msg
import Util

data InteractAliasStatus
    = HoldInteractAlias Secs
    | WaitReleaseInteractAlias

data StepsData = StepsData
    { _interactAliasStatus     :: InteractAliasStatus
    , _isGivenFreeRevolver     :: Bool
    , _stepsFakeProjectileId   :: MsgId
    , _isCurrentStepCompleted  :: Bool
    , _seenCycleLockOnEnemyIds :: S.Set MsgId
    }
