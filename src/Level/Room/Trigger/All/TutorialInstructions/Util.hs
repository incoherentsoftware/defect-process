module Level.Room.Trigger.All.TutorialInstructions.Util
    ( holdContinueSecs
    , instructionsStep1MaxN
    , TutorialInstructionsStep(..)
    ) where

import Util

holdContinueSecs      = 0.5 :: Secs
instructionsStep1MaxN = 2   :: Int

data TutorialInstructionsStep
    = InstructionsStep1 Int
    | InstructionsStep2
    | InstructionsStep2Completed
    | InstructionsStep3
    | InstructionsStep3Completed
    | InstructionsStep4
    deriving (Eq, Ord)
