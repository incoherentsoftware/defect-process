module Level.Room.Trigger.All.TutorialInstructions.StepsFakeProjectile
    ( StepsFakeProjectileData(..)
    , mkStepsFakeProjectile
    , updateStepsFakeProjectileMessage
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Collision
import Constants
import FileCache
import Level.Room.Trigger.All.TutorialInstructions.StepsData
import Level.Room.Trigger.All.TutorialInstructions.Util
import Msg
import Projectile
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

topText            = "Use lock-on targeting to auto-aim ranged attack"       :: T.Text
step1N0Text        = "(0/2) Cycle lock-on target: {LockOnSwitchTargetAlias}" :: T.Text
step1N1Text        = "(1/2) Cycle lock-on target: {LockOnSwitchTargetAlias}" :: T.Text
step1CompletedText = "(2/2) Cycle lock-on target: {LockOnSwitchTargetAlias}" :: T.Text
step2Text          = "Clear lock-on target: {LockOnClearAlias}"              :: T.Text

mouseKbStep3Text = "Set lock-on target at mouse cursor: {LockOnCursorAlias}" :: T.Text
gamepadStep3Text = "Set lock-on target manually aiming right analog stick"   :: T.Text

skipText         = "Skip: "               :: T.Text
continueText     = "Continue: "           :: T.Text
exitText         = "Exit Tutorial: "      :: T.Text
interactText     = "Hold {InteractAlias}" :: T.Text
rangedAttackText = "Ranged attack:"       :: T.Text
shootText        = "{ShootAlias}"         :: T.Text

topTextPosY           = 215.0 :: PosY
centerBackdropPosY    = 185.0 :: PosY
centerBackdropHeight1 = 136.0 :: Float
centerBackdropHeight2 = 206.0 :: Float
centerBackdropHeight3 = 276.0 :: Float

step1TextY = 265.0 :: PosY
step2TextY = 335.0 :: PosY
step3TextY = 405.0 :: PosY

interactTextOffsetY = 15.0 :: Float
interactRectHeight  = 1.0  :: Float
interactRectOffsetY = 38.0 :: PosY

rangedAttackTextPos = Pos2 1603.0 215.0 :: Pos2
shootTextPos        = Pos2 1603.0 260.0 :: Pos2
shootBackdropPosY   = 185.0             :: PosY
shootBackdropHeight = 105.0             :: Float

textBackdropBorderSize = 20.0                  :: Float
textBackdropColor      = Color 0 0 0 200       :: Color
textCompletedColor     = Color 150 150 150 255 :: Color
textSkipColor          = Color 210 210 210 255 :: Color

data StepsFakeProjectileData = StepsFakeProjectileData
    { _currentStep                           :: TutorialInstructionsStep
    , _stepsData                             :: StepsData
    , _topDisplayText                        :: DisplayText
    , _step1N0InputDisplayText               :: InputDisplayText
    , _step1N1InputDisplayText               :: InputDisplayText
    , _step1CompletedInputDisplayText        :: InputDisplayText
    , _step2InputDisplayText                 :: InputDisplayText
    , _step2CompletedInputDisplayText        :: InputDisplayText
    , _mouseKbStep3InputDisplayText          :: InputDisplayText
    , _mouseKbStep3CompletedInputDisplayText :: InputDisplayText
    , _gamepadStep3InputDisplayText          :: InputDisplayText
    , _gamepadStep3CompletedInputDisplayText :: InputDisplayText
    , _skipDisplayText                       :: DisplayText
    , _continueDisplayText                   :: DisplayText
    , _exitDisplayText                       :: DisplayText
    , _interactSkipInputDisplayText          :: InputDisplayText
    , _interactInputDisplayText              :: InputDisplayText
    , _rangedAttackDisplayText               :: DisplayText
    , _shootInputDisplayText                 :: InputDisplayText
    }

mkStepsFakeProjectileData
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => StepsData
    -> m StepsFakeProjectileData
mkStepsFakeProjectileData stepsData =
    StepsFakeProjectileData <$>
    pure (InstructionsStep1 0) <*>
    pure stepsData <*>
    mkDisplayText topText Font32 whiteColor <*>
    mkInputDisplayText ("{CheckboxSymbol} " <> step1N0Text) Font26 whiteColor <*>
    mkInputDisplayText ("{CheckboxSymbol} " <> step1N1Text) Font26 whiteColor <*>
    mkInputDisplayText ("{CheckboxFilledSymbol} " <> step1CompletedText) Font26 textCompletedColor <*>
    mkInputDisplayText ("{CheckboxSymbol} " <> step2Text) Font26 whiteColor <*>
    mkInputDisplayText ("{CheckboxFilledSymbol} " <> step2Text) Font26 textCompletedColor <*>
    mkInputDisplayText ("{CheckboxSymbol} " <> mouseKbStep3Text) Font26 whiteColor <*>
    mkInputDisplayText ("{CheckboxFilledSymbol} " <> mouseKbStep3Text) Font26 textCompletedColor <*>
    mkInputDisplayText ("{CheckboxSymbol} " <> gamepadStep3Text) Font26 whiteColor <*>
    mkInputDisplayText ("{CheckboxFilledSymbol} " <> gamepadStep3Text) Font26 textCompletedColor <*>
    mkDisplayText skipText Font26 textSkipColor <*>
    mkDisplayText continueText Font26 whiteColor <*>
    mkDisplayText exitText Font26 whiteColor <*>
    mkInputDisplayText interactText Font26 textSkipColor <*>
    mkInputDisplayText interactText Font26 whiteColor <*>
    mkDisplayText rangedAttackText Font26 whiteColor <*>
    mkInputDisplayText shootText Font26 whiteColor

mkStepsFakeProjectile
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => MsgId
    -> StepsData
    -> m (Some Projectile)
mkStepsFakeProjectile msgId stepsData = do
    stepsFakeProjData <- mkStepsFakeProjectileData stepsData
    let dummyHbx       = dummyHitbox zeroPos2

    return . Some $ (mkProjectile stepsFakeProjData msgId dummyHbx maxSecs)
        { _update = updateStepsFakeProjectile
        , _draw   = drawStepsFakeProjectile
        }

updateStepsFakeProjectile
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => ProjectileUpdate StepsFakeProjectileData m
updateStepsFakeProjectile stepsFakeProj = do
    let stepsFakeProjData = _data stepsFakeProj

    step1N0InputDisplayTxt        <- updateInputDisplayText $ _step1N0InputDisplayText stepsFakeProjData
    step1N1InputDisplayTxt        <- updateInputDisplayText $ _step1N1InputDisplayText stepsFakeProjData
    step1CompletedInputDisplayTxt <- updateInputDisplayText $ _step1CompletedInputDisplayText stepsFakeProjData
    step2InputDisplayTxt          <- updateInputDisplayText $ _step2InputDisplayText stepsFakeProjData
    step2CompletedInputDisplayTxt <- updateInputDisplayText $ _step2CompletedInputDisplayText stepsFakeProjData

    mouseKbStep3InputDisplayTxt          <- updateInputDisplayText $ _mouseKbStep3InputDisplayText stepsFakeProjData
    mouseKbStep3CompletedInputDisplayTxt <-
        updateInputDisplayText $ _mouseKbStep3CompletedInputDisplayText stepsFakeProjData
    gamepadStep3InputDisplayTxt          <- updateInputDisplayText $ _gamepadStep3InputDisplayText stepsFakeProjData
    gamepadStep3CompletedInputDisplayTxt <-
        updateInputDisplayText $ _gamepadStep3CompletedInputDisplayText stepsFakeProjData

    interactSkipInputDisplayTxt <- updateInputDisplayText $ _interactSkipInputDisplayText stepsFakeProjData
    interactInputDisplayTxt     <- updateInputDisplayText $ _interactInputDisplayText stepsFakeProjData
    shootInputDisplayTxt        <- updateInputDisplayText $ _shootInputDisplayText stepsFakeProjData

    return $ stepsFakeProj
        { _data = stepsFakeProjData
            { _step1N0InputDisplayText               = step1N0InputDisplayTxt
            , _step1N1InputDisplayText               = step1N1InputDisplayTxt
            , _step1CompletedInputDisplayText        = step1CompletedInputDisplayTxt
            , _step2InputDisplayText                 = step2InputDisplayTxt
            , _step2CompletedInputDisplayText        = step2CompletedInputDisplayTxt
            , _mouseKbStep3InputDisplayText          = mouseKbStep3InputDisplayTxt
            , _mouseKbStep3CompletedInputDisplayText = mouseKbStep3CompletedInputDisplayTxt
            , _gamepadStep3InputDisplayText          = gamepadStep3InputDisplayTxt
            , _gamepadStep3CompletedInputDisplayText = gamepadStep3CompletedInputDisplayTxt
            , _interactSkipInputDisplayText          = interactSkipInputDisplayTxt
            , _interactInputDisplayText              = interactInputDisplayTxt
            , _shootInputDisplayText                 = shootInputDisplayTxt
            }
        }

drawSteps123 :: (GraphicsReadWrite m, InputRead m, MonadIO m) => Float -> ProjectileDraw StepsFakeProjectileData m
drawSteps123 maxStepsTxtWidth stepsFakeProj =
    let
        stepsFakeProjData = _data stepsFakeProj
        currentStep       = _currentStep stepsFakeProjData
        stepsTextX        = virtualRenderWidth / 2.0 - maxStepsTxtWidth / 2.0
    in do
        -- step1
        when (currentStep >= InstructionsStep1 0) $
            let
                step1InputDisplayTxt = case currentStep of
                    InstructionsStep1 0 -> _step1N0InputDisplayText stepsFakeProjData
                    InstructionsStep1 1 -> _step1N1InputDisplayText stepsFakeProjData
                    _                   -> _step1CompletedInputDisplayText stepsFakeProjData
            in drawInputDisplayText (Pos2 stepsTextX step1TextY) uiInfoTextZIndex step1InputDisplayTxt

        -- step2
        when (currentStep >= InstructionsStep2) $
            let
                step2InputDisplayTxt
                    | currentStep == InstructionsStep2 = _step2InputDisplayText stepsFakeProjData
                    | otherwise                        = _step2CompletedInputDisplayText stepsFakeProjData
            in drawInputDisplayText (Pos2 stepsTextX step2TextY) uiInfoTextZIndex step2InputDisplayTxt

        -- step3
        lastUsedInputType <- _lastUsedInputType <$> readInputState
        when (currentStep >= InstructionsStep3) $ do
            let
                step3InputDisplayTxt = case lastUsedInputType of
                    MouseKbInputType
                        | currentStep == InstructionsStep3 -> _mouseKbStep3InputDisplayText stepsFakeProjData
                        | otherwise                        -> _mouseKbStep3CompletedInputDisplayText stepsFakeProjData
                    GamepadInputType
                        | currentStep == InstructionsStep3 -> _gamepadStep3InputDisplayText stepsFakeProjData
                        | otherwise                        -> _gamepadStep3CompletedInputDisplayText stepsFakeProjData
            drawInputDisplayText (Pos2 stepsTextX step3TextY) uiInfoTextZIndex step3InputDisplayTxt

drawInteractControls
    :: (GraphicsReadWrite m, InputRead m, MonadIO m)
    => PosY
    -> ProjectileDraw StepsFakeProjectileData m
drawInteractControls backdropRectBot stepsFakeProj =
    let
        stepsFakeProjData = _data stepsFakeProj
        currentStep       = _currentStep stepsFakeProjData

        interactDisplayTxt = case currentStep of
            InstructionsStep1 n
                | n < instructionsStep1MaxN -> _skipDisplayText stepsFakeProjData
                | otherwise                 -> _continueDisplayText stepsFakeProjData
            InstructionsStep2               -> _skipDisplayText stepsFakeProjData
            InstructionsStep2Completed      -> _continueDisplayText stepsFakeProjData
            InstructionsStep3               -> _skipDisplayText stepsFakeProjData
            InstructionsStep3Completed      -> _continueDisplayText stepsFakeProjData
            InstructionsStep4               -> _exitDisplayText stepsFakeProjData

        interactInputDisplayTxt = case currentStep of
            InstructionsStep1 _ -> _interactSkipInputDisplayText stepsFakeProjData
            InstructionsStep2   -> _interactSkipInputDisplayText stepsFakeProjData
            InstructionsStep3   -> _interactSkipInputDisplayText stepsFakeProjData
            _                   -> _interactInputDisplayText stepsFakeProjData
    in do
        displayTxtWidth      <- displayTextWidth interactDisplayTxt
        inputDisplayTxtWidth <- inputDisplayTextWidth interactInputDisplayTxt
        let
            totalTxtWidth    = displayTxtWidth + inputDisplayTxtWidth
            interactTextPosY = backdropRectBot + interactTextOffsetY
            text0Pos         = Pos2 (virtualRenderWidth / 2.0 - totalTxtWidth / 2.0) interactTextPosY
            text1Pos         = Pos2 (vecX text0Pos + displayTxtWidth) interactTextPosY

        drawDisplayText text0Pos uiInfoTextZIndex interactDisplayTxt
        drawInputDisplayText text1Pos uiInfoTextZIndex interactInputDisplayTxt

        interactInputDisplayTxtWidth <- inputDisplayTextWidth interactInputDisplayTxt
        let
            interactRectWidth = case _interactAliasStatus (_stepsData stepsFakeProjData) of
                HoldInteractAlias secs ->
                    let percent = min 1.0 (secs / holdContinueSecs)
                    in percent * interactInputDisplayTxtWidth
                _                      -> 0.0
        when (interactRectWidth > 0.0) $
            let pos = text1Pos `vecAdd` Pos2 0.0 interactRectOffsetY
            in drawRect pos interactRectWidth interactRectHeight whiteColor uiInfoTextZIndex

drawShootControls :: (GraphicsReadWrite m, InputRead m, MonadIO m) => ProjectileDraw StepsFakeProjectileData m
drawShootControls stepsFakeProj =
    let
        stepsFakeProjData      = _data stepsFakeProj
        rangedAttackDisplayTxt = _rangedAttackDisplayText stepsFakeProjData
        shootInputDisplayTxt   = _shootInputDisplayText stepsFakeProjData
    in do
        maxTxtWidth <- max <$> displayTextWidth rangedAttackDisplayTxt <*> inputDisplayTextWidth shootInputDisplayTxt
        let
            backdropWidth = maxTxtWidth + textBackdropBorderSize * 2.0
            backdropX     = vecX shootTextPos - backdropWidth / 2.0
            backdropPos   = Pos2 backdropX shootBackdropPosY

        drawRect backdropPos backdropWidth shootBackdropHeight textBackdropColor uiInfoTextZIndex
        drawDisplayTextCentered rangedAttackTextPos uiInfoTextZIndex rangedAttackDisplayTxt
        drawInputDisplayTextCentered shootTextPos uiInfoTextZIndex shootInputDisplayTxt

drawStepsFakeProjectile :: (GraphicsReadWrite m, InputRead m, MonadIO m) => ProjectileDraw StepsFakeProjectileData m
drawStepsFakeProjectile stepsFakeProj = do
    setCameraSpace CameraScreenSpace

    let
        stepsFakeProjData = _data stepsFakeProj
        currentStep       = _currentStep stepsFakeProjData

        topDisplayTxt               = _topDisplayText stepsFakeProjData
        mouseKbStep3InputDisplayTxt = _mouseKbStep3InputDisplayText stepsFakeProjData
        gamepadStep3InputDisplayTxt = _gamepadStep3InputDisplayText stepsFakeProjData

    step3TxtWidth       <- (_lastUsedInputType <$> readInputState) >>= \case
        MouseKbInputType -> inputDisplayTextWidth mouseKbStep3InputDisplayTxt
        GamepadInputType -> inputDisplayTextWidth gamepadStep3InputDisplayTxt
    maxStepsTxtWidth    <- fmap maximum . sequenceA $ NE.fromList
        [ inputDisplayTextWidth $ _step1N0InputDisplayText stepsFakeProjData
        , inputDisplayTextWidth $ _step1N1InputDisplayText stepsFakeProjData
        , inputDisplayTextWidth $ _step1CompletedInputDisplayText stepsFakeProjData
        , inputDisplayTextWidth $ _step2InputDisplayText stepsFakeProjData
        , pure step3TxtWidth
        ]
    maxBackdropTxtWidth <- max maxStepsTxtWidth <$> displayTextWidth topDisplayTxt

    let
        backdropRectWidth  = maxBackdropTxtWidth + textBackdropBorderSize * 2.0
        textCenterX        = virtualRenderWidth / 2.0
        topRectX           = textCenterX - backdropRectWidth / 2.0
        topRectPos         = Pos2 topRectX centerBackdropPosY
        backdropRectHeight = if
            | currentStep >= InstructionsStep3 -> centerBackdropHeight3
            | currentStep >= InstructionsStep2 -> centerBackdropHeight2
            | otherwise                        -> centerBackdropHeight1
    drawRect topRectPos backdropRectWidth backdropRectHeight textBackdropColor uiInfoTextZIndex

    drawDisplayTextCentered (Pos2 textCenterX topTextPosY) uiInfoTextZIndex topDisplayTxt
    drawSteps123 maxStepsTxtWidth stepsFakeProj
    drawInteractControls (centerBackdropPosY + backdropRectHeight) stepsFakeProj
    drawShootControls stepsFakeProj

    setCameraSpace CameraWorldSpace

updateStepsFakeProjectileMessage :: Maybe TutorialInstructionsStep -> StepsData -> Msg ThinkLevelMsgsPhase
updateStepsFakeProjectileMessage currentStep stepsData = mkMsgTo (ProjectileMsgUpdate updateProj) stepsFakeProjId
    where
        updateProj = \proj ->
            let projData = _data proj
            in proj
                { _data = projData
                    { _currentStep = fromMaybe (_currentStep projData) currentStep
                    , _stepsData   = stepsData
                    }
                }

        stepsFakeProjId = _stepsFakeProjectileId stepsData
