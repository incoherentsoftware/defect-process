module Player.TauntState.Types
    ( PlayerTauntState(..)
    ) where

import qualified Data.Set as S

import Attack.Description
import Msg.Types

data PlayerTauntState = PlayerTauntState
    { _tauntAttack           :: AttackDescription
    , _tauntedEnemyIds       :: S.Set MsgId
    , _queuedTauntedEnemyIds :: S.Set MsgId
    }
