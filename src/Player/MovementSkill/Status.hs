module Player.MovementSkill.Status
    ( MovementSkillStatus(..)
    , isMovementSkillStatusActive
    , isMovementSkillStatusCancelable
    , isMovementSkillStatusWalkCancelable
    ) where

data MovementSkillStatus
    = ActiveCancelableMovement
    | ActiveWalkCancelableMovement
    | ActiveNotCancelableMovement
    | ActiveAttackNotCancelableMovement
    | InactiveMovement
    deriving (Eq, Show)

isMovementSkillStatusActive :: MovementSkillStatus -> Bool
isMovementSkillStatusActive = \case
    ActiveCancelableMovement          -> True
    ActiveWalkCancelableMovement      -> True
    ActiveNotCancelableMovement       -> True
    ActiveAttackNotCancelableMovement -> True
    InactiveMovement                  -> False

isMovementSkillStatusCancelable :: MovementSkillStatus -> Bool
isMovementSkillStatusCancelable = \case
    ActiveCancelableMovement          -> True
    ActiveWalkCancelableMovement      -> False
    ActiveNotCancelableMovement       -> False
    ActiveAttackNotCancelableMovement -> False
    InactiveMovement                  -> True

isMovementSkillStatusWalkCancelable :: MovementSkillStatus -> Bool
isMovementSkillStatusWalkCancelable = \case
    ActiveCancelableMovement          -> False
    ActiveWalkCancelableMovement      -> True
    ActiveNotCancelableMovement       -> False
    ActiveAttackNotCancelableMovement -> False
    InactiveMovement                  -> True
