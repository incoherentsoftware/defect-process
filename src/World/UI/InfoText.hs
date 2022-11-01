module World.UI.InfoText
    ( InfoTextUI
    , mkInfoTextUI
    , updateInfoTextUI
    , drawInfoTextUI
    , resetInfoTextUIOnChangeWorldRoom
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import Data.Foldable          (for_)
import Data.Functor           ((<&>))
import qualified Data.Text as T

import Constants
import FileCache
import Msg
import Player.EquipmentInfo
import Player.SecondarySkill
import Util
import Window.Graphics
import Window.InputState
import World.UI.InfoText.Types
import World.ZIndex

moveControlsLeftText            = "Move left: {LeftAlias}"                   :: T.Text
moveControlsRightText           = "Move right: {RightAlias}"                 :: T.Text
equipmentInfoText               = "View equipment: {MenuAlias}"              :: T.Text
switchWeaponText                = "Switch melee weapon: {SwitchWeaponAlias}" :: T.Text
switchGunText                   = "Switch ranged weapon: {SwitchGunAlias}"   :: T.Text
secondarySkillInputText         = " : {SecondarySkillAlias}"                 :: T.Text
textBackdropColor               = Color 0 0 0 200                            :: Color
textBackdropBorderSize          = 20.0                                       :: Float
equipmentInfoTextAliveSecs      = 10.0                                       :: Secs
secondarySkillInfoTextAliveSecs = maxSecs                                    :: Secs

moveControlsBackdropPosY   = 350.0                                                       :: PosY
moveControlsBackdropHeight = 60.0                                                        :: Float
moveControlsTextSplitWidth = 100.0                                                       :: Float
moveControlsTextCenterY    = moveControlsBackdropPosY + moveControlsBackdropHeight / 2.0 :: PosY

equipmentInfoBackdropPosY         = 250.0 :: PosY
equipmentInfoBackdropHeightSingle = 60.0  :: Float
equipmentInfoBackdropHeightDouble = 110.0 :: Float
equipmentInfoText0PosY            = 280.0 :: PosY
equipmentInfoText1PosY            = 330.0 :: PosY

secondarySkillInfoBackdropPosY       = 150.0 :: PosY
secondarySkillInfoText0PosY          = 173.0 :: PosY
secondarySkillInfoTextOffsetY        = 56.0  :: OffsetY
secondarySkillInfoText3OffsetY       = 20.0  :: OffsetY
secondarySkillInfoBackdropBaseHeight = 75.0  :: Float
secondarySkillTextSpacerWidth        = 0.0   :: Float

secondarySkillSymbolBackdropPosY   = 150.0 :: PosY
secondarySkillSymbolText0PosY      = 173.0 :: PosY
secondarySkillSymbolBackdropHeight = 85.0  :: PosY
secondarySkillSymbolSpacerWidth    = 30.0  :: Float

formatSecondarySkillText :: Maybe SecondarySkillType -> T.Text
formatSecondarySkillText = \case
    Just t
        | isSecondarySkillTypeInAirOnly t -> " : " <> prettyShow t <> " (in air)"
        | otherwise                       -> " : " <> prettyShow t
    Nothing                               -> ""

updateSecondarySkillInfoTexts
    :: forall m. MsgsRead UpdateWorldUiMsgsPhase m
    => SecondarySkillInfoTexts
    -> m SecondarySkillInfoTexts
updateSecondarySkillInfoTexts secondarySkillInfoTxts = readPlayerEquipmentInfo <&> \playerEquipment ->
    let
        neutralText             = formatSecondarySkillText $ _secondarySkillNeutralType playerEquipment
        upText                  = formatSecondarySkillText $ _secondarySkillUpType playerEquipment
        downText                = formatSecondarySkillText $ _secondarySkillDownType playerEquipment
        neutralSymbolDisplayTxt = updateDisplayText neutralText (_neutral secondarySkillInfoTxts)
        upSymbolDisplayTxt      = updateDisplayText upText (_up secondarySkillInfoTxts)
        downSymbolDisplayTxt    = updateDisplayText downText (_down secondarySkillInfoTxts)
    in secondarySkillInfoTxts
        { _neutral = neutralSymbolDisplayTxt
        , _up      = upSymbolDisplayTxt
        , _down    = downSymbolDisplayTxt
        }

updateInfoTextType
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsRead UpdateWorldUiMsgsPhase m)
    => InfoTextType
    -> m InfoTextType
updateInfoTextType = \case
    NoInfoTextType -> return NoInfoTextType

    MoveControlsInfoTextType leftInputDisplayTxt rightInputDisplayTxt ->
        MoveControlsInfoTextType <$>
        updateInputDisplayText leftInputDisplayTxt <*>
        updateInputDisplayText rightInputDisplayTxt

    EquipmentInfoTextType ttl line0InputDisplayTxt line1InputDisplayTxt
        | ttl <= 0.0 -> return NoInfoTextType
        | otherwise  -> do
            let ttl'               = max 0.0 (ttl - timeStep)
            line1InputDisplayTxt' <- case line1InputDisplayTxt of
                Nothing  -> return Nothing
                Just idt -> Just <$> updateInputDisplayText idt

            id $
                EquipmentInfoTextType ttl' <$>
                updateInputDisplayText line0InputDisplayTxt <*>
                pure line1InputDisplayTxt'

    SecondarySkillInfoTextType ttl text3InputDisplayTxt secondarySkillInfoTxts
        | ttl <= 0.0 -> return NoInfoTextType
        | otherwise  -> do
            let ttl'                 = max 0.0 (ttl - timeStep)
            text3InputDisplayTxt'   <- updateInputDisplayText text3InputDisplayTxt
            secondarySkillInfoTxts' <- updateSecondarySkillInfoTexts secondarySkillInfoTxts
            return $ SecondarySkillInfoTextType ttl' text3InputDisplayTxt' secondarySkillInfoTxts'

mkInfoTextUI :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m) => m InfoTextUI
mkInfoTextUI =
    let
        mkInputDisplayText'  = \txt -> mkInputDisplayText txt Font32 whiteColor
        mkSymbolDisplayText' = \txt -> mkSymbolDisplayText txt Font32 whiteColor
    in do
        moveControlsLeftDisplayTxt                 <- mkInputDisplayText' moveControlsLeftText
        moveControlsRightDisplayTxt                <- mkInputDisplayText' moveControlsRightText
        equipmentInfoInputDisplayTxt               <- mkInputDisplayText' equipmentInfoText
        equipmentInfoSmallInputDisplayTxt          <- mkInputDisplayText equipmentInfoText Font22 whiteColor
        switchWeaponInputDisplayTxt                <- mkInputDisplayText' switchWeaponText
        switchGunInputDisplayTxt                   <- mkInputDisplayText' switchGunText
        secondarySkillInputDisplayTxt              <- mkInputDisplayText secondarySkillInputText Font26 whiteColor
        secondarySkillNeutralInputSymbolDisplayTxt <- mkSymbolDisplayText' "{SecondarySkillNeutralInputSymbol}"
        secondarySkillUpInputSymbolDisplayTxt      <- mkSymbolDisplayText' "{SecondarySkillUpInputSymbol}"
        secondarySkillDownInputSymbolDisplayTxt    <- mkSymbolDisplayText' "{SecondarySkillDownInputSymbol}"

        return $ InfoTextUI
            { _infoTextType                                = NoInfoTextType
            , _moveControlsLeftInputDisplayText            = moveControlsLeftDisplayTxt
            , _moveControlsRightInputDisplayText           = moveControlsRightDisplayTxt
            , _equipmentInfoInputDisplayText               = equipmentInfoInputDisplayTxt
            , _equipmentInfoSmallInputDisplayText          = equipmentInfoSmallInputDisplayTxt
            , _switchWeaponInputDisplayText                = switchWeaponInputDisplayTxt
            , _switchGunInputDisplayText                   = switchGunInputDisplayTxt
            , _secondarySkillInputDisplayText              = secondarySkillInputDisplayTxt
            , _secondarySkillNeutralInputSymbolDisplayText = secondarySkillNeutralInputSymbolDisplayTxt
            , _secondarySkillUpInputSymbolDisplayText      = secondarySkillUpInputSymbolDisplayTxt
            , _secondarySkillDownInputSymbolDisplayText    = secondarySkillDownInputSymbolDisplayTxt
            }

processUiMsgs
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsRead UpdateWorldUiMsgsPhase m)
    => InfoTextUI
    -> [UiMsgPayload]
    -> m InfoTextType
processUiMsgs infoTextUI [] = return $ case _infoTextType infoTextUI of
    MoveControlsInfoTextType _ _ -> NoInfoTextType
    infoTextType                 -> infoTextType
processUiMsgs infoTextUI (p:ps) = case p of
    UiMsgShowMoveControls -> return $
        let
            leftInputDisplayTxt  = _moveControlsLeftInputDisplayText infoTextUI
            rightInputDisplayTxt = _moveControlsRightInputDisplayText infoTextUI
        in MoveControlsInfoTextType leftInputDisplayTxt rightInputDisplayTxt

    UiMsgShowWeaponEquipmentInfo wpnCount -> return $
        let
            (line0DisplayTxt, line1DisplayTxt)
                | wpnCount > 1 = (_switchWeaponInputDisplayText infoTextUI, Just equipmentInfoInputDisplayTxt)
                | otherwise    = (equipmentInfoInputDisplayTxt, Nothing)
        in EquipmentInfoTextType equipmentInfoTextAliveSecs line0DisplayTxt line1DisplayTxt

    UiMsgShowGunEquipmentInfo gunCount -> return $
        let
            (line0DisplayTxt, line1DisplayTxt)
                | gunCount > 1 = (_switchGunInputDisplayText infoTextUI, Just equipmentInfoInputDisplayTxt)
                | otherwise    = (equipmentInfoInputDisplayTxt, Nothing)
        in EquipmentInfoTextType equipmentInfoTextAliveSecs line0DisplayTxt line1DisplayTxt

    UiMsgShowSecondarySkillEquipmentInfo playerEquipment -> do
        let
            neutralText = formatSecondarySkillText $ _secondarySkillNeutralType playerEquipment
            upText      = formatSecondarySkillText $ _secondarySkillUpType playerEquipment
            downText    = formatSecondarySkillText $ _secondarySkillDownType playerEquipment
        neutralDisplayTxt <- mkDisplayText neutralText Font26 whiteColor
        upDisplayTxt      <- mkDisplayText upText Font26 whiteColor
        downDisplayTxt    <- mkDisplayText downText Font26 whiteColor

        let
            equipmentInfoSmlInputDisplayTxt = _equipmentInfoSmallInputDisplayText infoTextUI
            ssInfoTexts                     = SecondarySkillInfoTexts
                { _neutral = neutralDisplayTxt
                , _up      = upDisplayTxt
                , _down    = downDisplayTxt
                }
        return $ SecondarySkillInfoTextType secondarySkillInfoTextAliveSecs equipmentInfoSmlInputDisplayTxt ssInfoTexts

    UiMsgShowGeneralEquipmentInfo -> return $
        EquipmentInfoTextType equipmentInfoTextAliveSecs equipmentInfoInputDisplayTxt Nothing

    UiMsgHideEquipmentInfo -> return NoInfoTextType

    _ -> processUiMsgs infoTextUI ps

    where equipmentInfoInputDisplayTxt = _equipmentInfoInputDisplayText infoTextUI

updateInfoTextUI
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsRead UpdateWorldUiMsgsPhase m)
    => InfoTextUI
    -> m InfoTextUI
updateInfoTextUI infoTextUI = do
    infoTextType  <- processUiMsgs infoTextUI =<< readMsgs
    infoTextType' <- updateInfoTextType infoTextType

    moveControlsLeftInputDisplayTxt   <- updateInputDisplayText $ _moveControlsLeftInputDisplayText infoTextUI
    moveControlsRightInputDisplayTxt  <- updateInputDisplayText $ _moveControlsRightInputDisplayText infoTextUI
    equipmentInfoInputDisplayTxt      <- updateInputDisplayText $ _equipmentInfoInputDisplayText infoTextUI
    equipmentInfoSmallInputDisplayTxt <- updateInputDisplayText $ _equipmentInfoSmallInputDisplayText infoTextUI
    switchWeaponInputDisplayTxt       <- updateInputDisplayText $ _switchWeaponInputDisplayText infoTextUI
    switchGunInputDisplayTxt          <- updateInputDisplayText $ _switchGunInputDisplayText infoTextUI
    secondarySkillInputDisplayTxt     <- updateInputDisplayText $ _secondarySkillInputDisplayText infoTextUI

    return $ infoTextUI
        { _infoTextType                       = infoTextType'
        , _moveControlsLeftInputDisplayText   = moveControlsLeftInputDisplayTxt
        , _moveControlsRightInputDisplayText  = moveControlsRightInputDisplayTxt
        , _equipmentInfoInputDisplayText      = equipmentInfoInputDisplayTxt
        , _equipmentInfoSmallInputDisplayText = equipmentInfoSmallInputDisplayTxt
        , _switchWeaponInputDisplayText       = switchWeaponInputDisplayTxt
        , _switchGunInputDisplayText          = switchGunInputDisplayTxt
        , _secondarySkillInputDisplayText     = secondarySkillInputDisplayTxt
        }

drawMoveControlsInfo :: (GraphicsReadWrite m, InputRead m, MonadIO m) => InputDisplayText -> InputDisplayText -> m ()
drawMoveControlsInfo leftInputDisplayTxt rightInputDisplayTxt = do
    leftRectWidth <- (+ (textBackdropBorderSize * 2.0)) <$> inputDisplayTextWidth leftInputDisplayTxt
    let
        textCenterX = virtualRenderWidth / 2.0
        leftRectX   = textCenterX - moveControlsTextSplitWidth / 2.0 - leftRectWidth
        leftRectPos = Pos2 leftRectX moveControlsBackdropPosY
    drawRect leftRectPos leftRectWidth moveControlsBackdropHeight textBackdropColor uiInfoTextZIndex

    rightRectWidth <- (+ (textBackdropBorderSize * 2.0)) <$> inputDisplayTextWidth rightInputDisplayTxt
    let
        rightRectX   = textCenterX + moveControlsTextSplitWidth / 2.0
        rightRectPos = Pos2 rightRectX moveControlsBackdropPosY
    drawRect rightRectPos rightRectWidth moveControlsBackdropHeight textBackdropColor uiInfoTextZIndex

    let
        leftTextPos  = Pos2 (leftRectX + leftRectWidth / 2.0) moveControlsTextCenterY
        rightTextPos = Pos2 (rightRectX + rightRectWidth / 2.0) moveControlsTextCenterY
    drawInputDisplayTextCentered leftTextPos uiInfoTextZIndex leftInputDisplayTxt
    drawInputDisplayTextCentered rightTextPos uiInfoTextZIndex rightInputDisplayTxt

drawEquipmentInfo
    :: (GraphicsReadWrite m, InputRead m, MonadIO m)
    => InputDisplayText
    -> Maybe InputDisplayText
    -> m ()
drawEquipmentInfo line0InputDisplayTxt line1InputDisplayTxt = case line1InputDisplayTxt of
    Nothing -> do
        rectWidth <- (+ (textBackdropBorderSize * 2.0)) <$> inputDisplayTextWidth line0InputDisplayTxt
        let
            textCenterX = virtualRenderWidth / 2.0
            rectX       = textCenterX - rectWidth / 2.0
            rectPos     = Pos2 rectX equipmentInfoBackdropPosY
        drawRect rectPos rectWidth equipmentInfoBackdropHeightSingle textBackdropColor uiInfoTextZIndex

        let textPos = Pos2 (rectX + rectWidth / 2.0) equipmentInfoText0PosY
        drawInputDisplayTextCentered textPos uiInfoTextZIndex line0InputDisplayTxt

    Just line1InputDisplayTxt' -> do
        widths <- sequenceA
            [ inputDisplayTextWidth line0InputDisplayTxt
            , inputDisplayTextWidth line1InputDisplayTxt'
            ]

        let
            rectWidth   = fromMaybe 0.0 (maybeMaximum widths) + textBackdropBorderSize * 2.0
            textCenterX = virtualRenderWidth / 2.0
            rectX       = textCenterX - rectWidth / 2.0
            rectPos     = Pos2 rectX equipmentInfoBackdropPosY
        drawRect rectPos rectWidth equipmentInfoBackdropHeightDouble textBackdropColor uiInfoTextZIndex

        let
            text0Pos = Pos2 (rectX + rectWidth / 2.0) equipmentInfoText0PosY
            text1Pos = Pos2 (rectX + rectWidth / 2.0) equipmentInfoText1PosY
        drawInputDisplayTextCentered text0Pos uiInfoTextZIndex line0InputDisplayTxt
        drawInputDisplayTextCentered text1Pos uiInfoTextZIndex line1InputDisplayTxt'

secondarySkillInfoSymbolRectOffset :: [(SymbolDisplayText, DisplayText)] -> OffsetY
secondarySkillInfoSymbolRectOffset skillInfoLines = case length skillInfoLines of
    0 -> 0.0
    1 -> 0.0
    2 -> secondarySkillInfoTextOffsetY / 2.0
    _ -> secondarySkillInfoTextOffsetY

drawSecondarySkillInfo
    :: (GraphicsReadWrite m, InputRead m, MonadIO m)
    => InputDisplayText
    -> SecondarySkillInfoTexts
    -> InfoTextUI
    -> m ()
drawSecondarySkillInfo text3InputDisplayTxt secondarySkillInfoTxts infoTextUI = do
    let
        inputSymbolDisplayTxts =
            [ _secondarySkillNeutralInputSymbolDisplayText infoTextUI
            , _secondarySkillUpInputSymbolDisplayText infoTextUI
            , _secondarySkillDownInputSymbolDisplayText infoTextUI
            ]
        skillDisplayTxts       =
            [ _neutral secondarySkillInfoTxts
            , _up secondarySkillInfoTxts
            , _down secondarySkillInfoTxts
            ]
        skillInfoLines         =
            [ (symbolDisplayTxt, displayTxt)
            | (symbolDisplayTxt, displayTxt) <- zip inputSymbolDisplayTxts skillDisplayTxts
            , _text (displayTxt :: DisplayText) /= ""
            ]
        skillInfoLinesHeight   = fromIntegral (length skillInfoLines) * secondarySkillInfoTextOffsetY
        skillInfoLastLinePosY  = secondarySkillInfoText0PosY + skillInfoLinesHeight

    symbolDisplayTxtWidths <- traverse (symbolDisplayTextWidth . fst) skillInfoLines
    let
        imgWidth           = fromMaybe 0.0 (maybeMaximum symbolDisplayTxtWidths)
        symbolLeftPadWidth = imgWidth + secondarySkillTextSpacerWidth

    symbolWidths <- map (symbolLeftPadWidth +) <$> traverse displayTextWidth skillDisplayTxts
    widths       <- (:symbolWidths) <$> inputDisplayTextWidth text3InputDisplayTxt

    let
        rectWidth   = fromMaybe 0.0 (maybeMaximum widths) + textBackdropBorderSize * 2.0
        textCenterX = virtualRenderWidth / 2.0
        rectX       = textCenterX - rectWidth / 2.0
        rectPos     = Pos2 rectX secondarySkillInfoBackdropPosY
        rectHeight  = secondarySkillInfoBackdropBaseHeight + skillInfoLinesHeight
    drawRect rectPos rectWidth rectHeight textBackdropColor uiInfoTextZIndex

    let
        symbolMaxWidth = fromMaybe 0.0 (maybeMaximum symbolWidths)
        imgLeft        = rectX + (rectWidth - symbolMaxWidth) / 2.0
        imgRight       = imgLeft + imgWidth + secondarySkillTextSpacerWidth
        rectCenterX    = rectX + rectWidth / 2.0

    for_ (zip [0..] skillInfoLines) $ \(i, (inputSymbolDisplayTxt, skillDisplayTxt)) ->
        let
            x   = fromIntegral $ round imgRight
            y   = secondarySkillInfoText0PosY + i * secondarySkillInfoTextOffsetY
            pos = Pos2 x y
        in do
            drawSymbolDisplayTextRightAligned pos uiInfoTextZIndex inputSymbolDisplayTxt
            drawDisplayText pos uiInfoTextZIndex skillDisplayTxt

    let text3Pos = Pos2 rectCenterX (skillInfoLastLinePosY + secondarySkillInfoText3OffsetY)
    drawInputDisplayTextCentered text3Pos uiInfoTextZIndex text3InputDisplayTxt

    let
        neutralInputSymbolDisplayTxt        = _secondarySkillNeutralInputSymbolDisplayText infoTextUI
        secondarySkillSymbolDisplayTxtWidth = symbolDisplayTextImageWidth neutralInputSymbolDisplayTxt
        secondarySkillInputDisplayTxt       = _secondarySkillInputDisplayText infoTextUI
    secondarySkillInputDisplayTxtWidth <- inputDisplayTextWidth secondarySkillInputDisplayTxt
    let
        symbolWidth       = secondarySkillSymbolDisplayTxtWidth + secondarySkillInputDisplayTxtWidth
        symbolRectWidth   = symbolWidth + textBackdropBorderSize * 2.0
        symbolRectX       = fromIntegral $ round (rectX + rectWidth + secondarySkillSymbolSpacerWidth)
        symbolRectOffsetY = secondarySkillInfoSymbolRectOffset skillInfoLines
        symbolRectPos     = Pos2 symbolRectX (secondarySkillSymbolBackdropPosY + symbolRectOffsetY)
    drawRect symbolRectPos symbolRectWidth secondarySkillSymbolBackdropHeight textBackdropColor uiInfoTextZIndex

    let
        symbolTextX   = symbolRectX + textBackdropBorderSize
        symbolTextY   = secondarySkillSymbolText0PosY + symbolRectOffsetY
        symbolTextPos = Pos2 symbolTextX symbolTextY
        inputTextPos  = Pos2 (symbolTextX + secondarySkillSymbolDisplayTxtWidth) symbolTextY
    drawSymbolDisplayText symbolTextPos uiInfoTextZIndex neutralInputSymbolDisplayTxt
    drawInputDisplayText inputTextPos uiInfoTextZIndex secondarySkillInputDisplayTxt

drawInfoTextUI :: (GraphicsReadWrite m, InputRead m, MonadIO m) => InfoTextUI -> m ()
drawInfoTextUI infoTextUI = case _infoTextType infoTextUI of
    NoInfoTextType -> return ()

    MoveControlsInfoTextType leftInputDisplayTxt rightInputDisplayTxt ->
        drawMoveControlsInfo leftInputDisplayTxt rightInputDisplayTxt

    EquipmentInfoTextType _ line0InputDisplayTxt line1InputDisplayTxt ->
        drawEquipmentInfo line0InputDisplayTxt line1InputDisplayTxt

    SecondarySkillInfoTextType _ text3InputDisplayTxt secondarySkillInfoTxts ->
        drawSecondarySkillInfo text3InputDisplayTxt secondarySkillInfoTxts infoTextUI

resetInfoTextUIOnChangeWorldRoom :: InfoTextUI -> InfoTextUI
resetInfoTextUIOnChangeWorldRoom infoTextUI = infoTextUI {_infoTextType = NoInfoTextType}
