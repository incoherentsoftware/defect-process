module Attack.Hit
    ( mkAttackHit
    , mkAttackHitEx
    , mkAttackHitEmpty
    ) where

import Attack.Hit.Types
import Attack.Types
import Msg
import Util

mkAttackHit      :: Attack -> AttackHit
mkAttackHitEx    :: Pos2 -> Attack -> AttackHit
mkAttackHitEmpty :: MsgId -> Pos2 -> AttackHit
