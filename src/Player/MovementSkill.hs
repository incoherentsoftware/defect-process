module Player.MovementSkill
    ( module Player.MovementSkill.Types
    , module Player.MovementSkill.Status
    , mkMovementSkill
    , movementSkillActive
    , cancelMovementSkill
    , movementSkillCancelable
    , movementSkillWalkCancelable
    , updateMovementSkill
    ) where

import Control.Monad.State (execStateT, get, lift, modify, put, when)
import Data.Dynamic        (fromDynamic)
import Data.Typeable       (Typeable)

import AppEnv
import Msg.Phase
import Player.Flags
import Player.MovementSkill.Status
import Player.MovementSkill.Types
import Player.Types
import {-# SOURCE #-} Player

mkMovementSkill :: Typeable d => d -> MovementSkillType -> MovementSkill d
mkMovementSkill dat moveSkillType = MovementSkill
    { _data             = dat
    , _type             = moveSkillType
    , _status           = InactiveMovement
    , _sprite           = Nothing
    , _cooldown         = 0.0
    , _numCharges       = 1
    , _canRefreshCharge = False
    , _think            = \_ _ _ -> return []
    , _update           = const $ return . id
    , _updateDynamic    = updateDynamic
    , _draw             = \_ _ -> return ()
    }

updateDynamic :: Typeable d => MovementSkillUpdateDynamic d
updateDynamic dyn moveSkill = case fromDynamic dyn of
    Just update -> update moveSkill
    Nothing     -> moveSkill

movementSkillActive :: MovementSkill d -> Bool
movementSkillActive = isMovementSkillStatusActive . _status

cancelMovementSkill :: MovementSkill d -> MovementSkill d
cancelMovementSkill moveSkill = moveSkill {_status = InactiveMovement}

movementSkillCancelable :: MovementSkill d -> Bool
movementSkillCancelable = isMovementSkillStatusCancelable . _status

movementSkillWalkCancelable :: MovementSkill d -> Bool
movementSkillWalkCancelable = isMovementSkillStatusWalkCancelable . _status

updateMovementSkill :: Player -> MovementSkill d -> AppEnv UpdatePlayerMsgsPhase (MovementSkill d)
updateMovementSkill player moveSkill =
    let
        moveSkillActive     = movementSkillActive moveSkill
        moveSkillCancelable = movementSkillCancelable moveSkill
        flags               = _flags player
        jumped              = _jumped flags
        firedGun            = _firedGun flags
        isAttacking         = playerAttackActive player
        cancelSkill         = jumped || firedGun || isAttacking
    in flip execStateT moveSkill $ do
        when (moveSkillActive && moveSkillCancelable && cancelSkill) $
            modify cancelMovementSkill
        get >>= \ms -> lift ((_update ms) player ms) >>= put
