module Level.Room.Trigger.All.TutorialInstructions
    ( mkTutorialInstructionsTrigger
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))
import qualified Data.Set as S

import Constants
import Id
import InfoMsg.Util
import Level.Room.Trigger
import Level.Room.Trigger.All.TutorialInstructions.StepsData
import Level.Room.Trigger.All.TutorialInstructions.StepsFakeProjectile
import Level.Room.Trigger.All.TutorialInstructions.Util
import Level.Room.Trigger.Util
import Level.Room.Util
import Msg
import Player.Gun.Types
import Player.LockOnAim
import Window.InputState

checkboxFilledSoundPath = "event:/SFX Events/UI/tutorial-checkbox-filled" :: FilePath
continueSoundPath       = "event:/SFX Events/UI/tutorial-continue"        :: FilePath
skipSoundPath           = continueSoundPath                               :: FilePath
--skipSoundPath           = "event:/SFX Events/UI/tutorial-skip"            :: FilePath

mkTutorialInstructionsTrigger :: (MonadIO m, MsgsWrite UpdateLevelMsgsPhase m) => Bool -> m RoomTrigger
mkTutorialInstructionsTrigger isGivenFreeRevolver = do
    stepsFakeProjectileId <- newId
    let
        stepsData = StepsData
            { _interactAliasStatus     = WaitReleaseInteractAlias
            , _isGivenFreeRevolver     = isGivenFreeRevolver
            , _stepsFakeProjectileId   = stepsFakeProjectileId
            , _isCurrentStepCompleted  = False
            , _seenCycleLockOnEnemyIds = S.empty
            }

    writeMsgs [mkMsg $ NewUpdateProjectileMsgAddM (mkStepsFakeProjectile stepsFakeProjectileId stepsData)]

    trigger <- mkRoomTrigger
    return $ (trigger :: RoomTrigger) {_think = thinkStep0 stepsData}

readIsInteractAliasHold :: InputRead m => m Bool
readIsInteractAliasHold = (InteractAlias `aliasHold`) <$> readInputState

thinkStep0 :: Monad m => StepsData -> RoomTriggerThink m
thinkStep0 stepsData _ trigger = return
    [ updateTriggerThinkMessage (thinkStep1 stepsData) trigger
    , updateStepsFakeProjectileMessage (Just $ InstructionsStep1 0) stepsData
    ]

readPlayerEnemyLockOn :: MsgsRead ThinkLevelMsgsPhase m => m (Maybe PlayerEnemyLockOn)
readPlayerEnemyLockOn = processInfoMsgs <$> readMsgs
    where
        processInfoMsgs :: [InfoMsgPayload] -> Maybe PlayerEnemyLockOn
        processInfoMsgs []     = Nothing
        processInfoMsgs (p:ps) = case p of
            InfoMsgPlayer playerInfo -> _enemyLockOn (playerInfo :: PlayerInfo)
            _                        -> processInfoMsgs ps

readStep1SeenCycleLockOnEnemyIds :: MsgsRead ThinkLevelMsgsPhase m => StepsData -> m (S.Set MsgId)
readStep1SeenCycleLockOnEnemyIds stepsData =
    let seenCycleLockOnEnemyIds = _seenCycleLockOnEnemyIds stepsData
    in readPlayerEnemyLockOn <&> \case
        Just lockOn
            | _source lockOn == CycleLockOnSource -> _enemyId lockOn `S.insert` seenCycleLockOnEnemyIds
        _                                         -> seenCycleLockOnEnemyIds

thinkStep1 :: (InputRead m, MsgsRead ThinkLevelMsgsPhase m) => StepsData -> RoomTriggerThink m
thinkStep1 stepsData _ trigger = do
    seenCycleLockOnEnemyIds      <- readStep1SeenCycleLockOnEnemyIds stepsData
    let numSeenCycleLockOnEnemies = S.size seenCycleLockOnEnemyIds
    isInteractAliasHold          <- readIsInteractAliasHold

    return $ case _interactAliasStatus stepsData of
        _
            | numSeenCycleLockOnEnemies >= instructionsStep1MaxN && not (_isCurrentStepCompleted stepsData) ->
                let
                    stepsData' = stepsData
                        { _isCurrentStepCompleted  = True
                        , _seenCycleLockOnEnemyIds = seenCycleLockOnEnemyIds
                        }
                    step       = InstructionsStep1 numSeenCycleLockOnEnemies
                in
                    [ updateTriggerThinkMessage (thinkStep1 stepsData') trigger
                    , updateStepsFakeProjectileMessage (Just step) stepsData'
                    , mkMsg $ AudioMsgPlaySoundCentered checkboxFilledSoundPath
                    ]

        WaitReleaseInteractAlias
            | isInteractAliasHold -> []
            | otherwise           ->
                let
                    stepsData' = stepsData
                        { _interactAliasStatus     = HoldInteractAlias timeStep
                        , _seenCycleLockOnEnemyIds = seenCycleLockOnEnemyIds
                        }
                    step       = InstructionsStep1 numSeenCycleLockOnEnemies
                in
                    [ updateTriggerThinkMessage (thinkStep1 stepsData') trigger
                    , updateStepsFakeProjectileMessage (Just step) stepsData'
                    ]

        HoldInteractAlias secs
            | not isInteractAliasHold ->
                let
                    stepsData' = stepsData
                        { _interactAliasStatus     = HoldInteractAlias 0.0
                        , _seenCycleLockOnEnemyIds = seenCycleLockOnEnemyIds
                        }
                    step       = InstructionsStep1 numSeenCycleLockOnEnemies
                in
                    [ updateTriggerThinkMessage (thinkStep1 stepsData') trigger
                    , updateStepsFakeProjectileMessage (Just step) stepsData'
                    ]

            | otherwise -> if
                | secs < holdContinueSecs ->
                    let
                        stepsData' = stepsData
                            { _interactAliasStatus     = HoldInteractAlias (secs + timeStep)
                            , _seenCycleLockOnEnemyIds = seenCycleLockOnEnemyIds
                            }
                        step       = InstructionsStep1 numSeenCycleLockOnEnemies
                    in
                        [ updateTriggerThinkMessage (thinkStep1 stepsData') trigger
                        , updateStepsFakeProjectileMessage (Just step) stepsData'
                        ]

                | otherwise ->
                    let
                        interactSoundPath
                            | _isCurrentStepCompleted stepsData = continueSoundPath
                            | otherwise                         = skipSoundPath

                        stepsData' = stepsData
                            { _interactAliasStatus     = WaitReleaseInteractAlias
                            , _isCurrentStepCompleted  = False
                            , _seenCycleLockOnEnemyIds = seenCycleLockOnEnemyIds
                            }
                    in
                        [ updateTriggerThinkMessage (thinkStep2 stepsData') trigger
                        , updateStepsFakeProjectileMessage (Just InstructionsStep2) stepsData'
                        , mkMsg $ AudioMsgPlaySoundCentered interactSoundPath
                        ]

readStep2Completed :: InputRead m => m Bool
readStep2Completed = do
    inputState <- readInputState
    return $ LockOnClearAlias `aliasPressed` inputState

thinkStep2 :: InputRead m => StepsData -> RoomTriggerThink m
thinkStep2 stepsData _ trigger = do
    isStep2Completed    <- readStep2Completed
    isInteractAliasHold <- readIsInteractAliasHold

    return $ case _interactAliasStatus stepsData of
        _
            | isStep2Completed && not (_isCurrentStepCompleted stepsData) ->
                let stepsData' = stepsData {_isCurrentStepCompleted = True}
                in
                    [ updateTriggerThinkMessage (thinkStep2 stepsData') trigger
                    , updateStepsFakeProjectileMessage (Just InstructionsStep2Completed) stepsData'
                    , mkMsg $ AudioMsgPlaySoundCentered checkboxFilledSoundPath
                    ]

        WaitReleaseInteractAlias
            | isInteractAliasHold -> []
            | otherwise           ->
                let stepsData' = stepsData {_interactAliasStatus = HoldInteractAlias timeStep}
                in [updateTriggerThinkMessage (thinkStep2 stepsData') trigger]

        HoldInteractAlias secs
            | not isInteractAliasHold ->
                let stepsData' = stepsData {_interactAliasStatus = HoldInteractAlias 0.0}
                in
                    [ updateTriggerThinkMessage (thinkStep2 stepsData') trigger
                    , updateStepsFakeProjectileMessage Nothing stepsData'
                    ]

            | otherwise -> if
                | secs < holdContinueSecs ->
                    let stepsData' = stepsData {_interactAliasStatus = HoldInteractAlias (secs + timeStep)}
                    in
                        [ updateTriggerThinkMessage (thinkStep2 stepsData') trigger
                        , updateStepsFakeProjectileMessage Nothing stepsData'
                        ]

                | otherwise ->
                    let
                        interactSoundPath
                            | _isCurrentStepCompleted stepsData = continueSoundPath
                            | otherwise                         = skipSoundPath

                        stepsData' = stepsData
                            { _interactAliasStatus    = WaitReleaseInteractAlias
                            , _isCurrentStepCompleted = False
                            }
                    in
                        [ updateTriggerThinkMessage (thinkStep3 stepsData') trigger
                        , updateStepsFakeProjectileMessage (Just InstructionsStep3) stepsData'
                        , mkMsg $ AudioMsgPlaySoundCentered interactSoundPath
                        ]

readStep3Completed :: (InputRead m, MsgsRead ThinkLevelMsgsPhase m) => m Bool
readStep3Completed = readPlayerEnemyLockOn >>= \case
    Nothing       -> return False
    Just enLockOn -> (_lastUsedInputType <$> readInputState) <&> \case
        MouseKbInputType -> _source enLockOn == CursorLockOnSource
        GamepadInputType -> _source enLockOn == GamepadAxisLockOnSource

thinkStep3 :: (InputRead m, MsgsRead ThinkLevelMsgsPhase m) => StepsData -> RoomTriggerThink m
thinkStep3 stepsData _ trigger = do
    isStep3Completed    <- readStep3Completed
    isInteractAliasHold <- readIsInteractAliasHold

    return $ case _interactAliasStatus stepsData of
        _
            | isStep3Completed ->
                let stepsData' = stepsData {_interactAliasStatus = WaitReleaseInteractAlias}
                in
                    [ updateTriggerThinkMessage (thinkStep4 stepsData') trigger
                    , updateStepsFakeProjectileMessage (Just InstructionsStep4) stepsData'
                    , mkMsg $ AudioMsgPlaySoundCentered checkboxFilledSoundPath
                    ]

        WaitReleaseInteractAlias
            | isInteractAliasHold -> []
            | otherwise           ->
                let stepsData' = stepsData {_interactAliasStatus = HoldInteractAlias timeStep}
                in [updateTriggerThinkMessage (thinkStep3 stepsData') trigger]

        HoldInteractAlias secs
            | not isInteractAliasHold ->
                let stepsData' = stepsData {_interactAliasStatus = HoldInteractAlias 0.0}
                in
                    [ updateTriggerThinkMessage (thinkStep3 stepsData') trigger
                    , updateStepsFakeProjectileMessage Nothing stepsData'
                    ]

            | otherwise -> if
                | secs < holdContinueSecs ->
                    let stepsData' = stepsData {_interactAliasStatus = HoldInteractAlias (secs + timeStep)}
                    in
                        [ updateTriggerThinkMessage (thinkStep3 stepsData') trigger
                        , updateStepsFakeProjectileMessage Nothing stepsData'
                        ]

                | otherwise ->
                    let
                        interactSoundPath
                            | _isCurrentStepCompleted stepsData = continueSoundPath
                            | otherwise                         = skipSoundPath

                        stepsData' = stepsData {_interactAliasStatus = WaitReleaseInteractAlias}
                    in
                        [ updateTriggerThinkMessage (thinkStep4 stepsData') trigger
                        , updateStepsFakeProjectileMessage (Just InstructionsStep4) stepsData'
                        , mkMsg $ AudioMsgPlaySoundCentered interactSoundPath
                        ]

thinkStep4 :: InputRead m => StepsData -> RoomTriggerThink m
thinkStep4 stepsData _ trigger = do
    isInteractAliasHold <- readIsInteractAliasHold

    return $ case _interactAliasStatus stepsData of
        WaitReleaseInteractAlias
            | isInteractAliasHold -> []
            | otherwise           ->
                let stepsData' = stepsData {_interactAliasStatus = HoldInteractAlias timeStep}
                in [updateTriggerThinkMessage (thinkStep4 stepsData') trigger]

        HoldInteractAlias secs
            | not isInteractAliasHold ->
                let stepsData' = stepsData {_interactAliasStatus = HoldInteractAlias 0.0}
                in
                    [ updateTriggerThinkMessage (thinkStep4 stepsData') trigger
                    , updateStepsFakeProjectileMessage Nothing stepsData'
                    ]

            | otherwise -> if
                | secs < holdContinueSecs ->
                    let stepsData' = stepsData {_interactAliasStatus = HoldInteractAlias (secs + timeStep)}
                    in
                        [ updateTriggerThinkMessage (thinkStep4 stepsData') trigger
                        , updateStepsFakeProjectileMessage Nothing stepsData'
                        ]

                | otherwise ->
                    let
                        clearFreeEquipmentMsgs
                            | _isGivenFreeRevolver stepsData = [mkMsg $ PlayerMsgClearGun RevolverGun]
                            | otherwise                      = []
                    in
                        [ mkMsg $ WorldMsgSwitchRoom startingShopRoomType 0.0
                        , mkMsg PlayerMsgFillMeterFull
                        , mkMsg $ AudioMsgPlaySoundCentered continueSoundPath
                        ] ++ clearFreeEquipmentMsgs
