module World.UI.InfoText
    ( InfoTextUI
    , mkInfoTextUI
    , updateInfoTextUI
    , drawInfoTextUI
    , resetInfoTextUIOnChangeWorldRoom
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import Data.Functor           ((<&>))
import qualified Data.Text as T

import Constants
import FileCache
import InfoMsg.Util
import Msg
import Player.EquipmentInfo.Types
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
secondarySkillSymbolText        = "{SecondarySkillSymbol}"                   :: T.Text
secondarySkillInputText         = " : {SecondarySkillAlias}"                 :: T.Text
secondarySkillColonText         = " :"                                       :: T.Text
secondarySkillLiteralEmptyText  = "empty"                                    :: T.Text
textLiteralEmptyColor           = Color 100 100 100 255                      :: Color
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

secondarySkillInfoBackdropPosY   = 150.0         :: PosY
secondarySkillInfoImagePosY      = 166.0         :: PosY
secondarySkillInfoText0PosY      = 173.0         :: PosY
secondarySkillInfoText1PosY      = 229.0         :: PosY
secondarySkillInfoText2PosY      = 285.0         :: PosY
secondarySkillInfoText3PosY      = 363.0         :: PosY
secondarySkillEmptyTextOffset    = Pos2 20.0 0.0 :: Pos2
secondarySkillInfoBackdropHeight = 243.0         :: Float
secondarySkillTextSpacerWidth    = 0.0           :: Float

secondarySkillSymbolBackdropPosY   = 214.0 :: PosY
secondarySkillSymbolText0PosY      = 227.0 :: PosY
secondarySkillSymbolBackdropHeight = 65.0  :: PosY
secondarySkillSymbolSpacerWidth    = 30.0  :: Float

secondarySkillOverlayImagePath =
    PackResourceFilePath "data/ui/ui.pack" "secondary-skill-view-info-overlay.image" :: PackResourceFilePath

formatSecondarySkillText :: Maybe SecondarySkillType -> T.Text
formatSecondarySkillText = \case
    Just t
        | isSecondarySkillTypeInAirOnly t -> " : " <> prettyShow t <> " (in air)"
        | otherwise                       -> " : " <> prettyShow t
    Nothing                               -> secondarySkillColonText

updateSecondarySkillInfoTexts
    :: forall m. MsgsRead UpdateWorldUiMsgsPhase m
    => SecondarySkillInfoTexts
    -> m SecondarySkillInfoTexts
updateSecondarySkillInfoTexts secondarySkillInfoTxts =
    let
        readPlayerEquipmentInfo :: m (Maybe PlayerEquipmentInfo)
        readPlayerEquipmentInfo = processMsgs <$> readMsgs
            where
                processMsgs :: [InfoMsgPayload] -> Maybe PlayerEquipmentInfo
                processMsgs []     = Nothing
                processMsgs (p:ps) = case p of
                    InfoMsgPlayer playerInfo -> Just $ _equipment playerInfo
                    _                        -> processMsgs ps
    in readPlayerEquipmentInfo <&> \case
        Nothing              -> secondarySkillInfoTxts
        Just playerEquipment ->
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
mkInfoTextUI = do
    let mkInputDisplayText' = \txt -> mkInputDisplayText txt Font32 whiteColor

    moveControlsLeftDisplayTxt           <- mkInputDisplayText' moveControlsLeftText
    moveControlsRightDisplayTxt          <- mkInputDisplayText' moveControlsRightText
    equipmentInfoInputDisplayTxt         <- mkInputDisplayText' equipmentInfoText
    switchWeaponInputDisplayTxt          <- mkInputDisplayText' switchWeaponText
    switchGunInputDisplayTxt             <- mkInputDisplayText' switchGunText
    secondarySkillLiteralEmptyDisplayTxt <- mkDisplayText secondarySkillLiteralEmptyText Font26 textLiteralEmptyColor
    secondarySkillSymbolDisplayTxt       <- mkSymbolDisplayText secondarySkillSymbolText Font32 whiteColor
    secondarySkillInputDisplayTxt        <- mkInputDisplayText' secondarySkillInputText
    secondarySkillOverlayImg             <- loadPackImage secondarySkillOverlayImagePath

    return $ InfoTextUI
        { _infoTextType                      = NoInfoTextType
        , _moveControlsLeftInputDisplayText  = moveControlsLeftDisplayTxt
        , _moveControlsRightInputDisplayText = moveControlsRightDisplayTxt
        , _equipmentInfoInputDisplayText     = equipmentInfoInputDisplayTxt
        , _switchWeaponInputDisplayText      = switchWeaponInputDisplayTxt
        , _switchGunInputDisplayText         = switchGunInputDisplayTxt
        , _secondarySkillLiteralEmptyText    = secondarySkillLiteralEmptyDisplayTxt
        , _secondarySkillSymbolDisplayText   = secondarySkillSymbolDisplayTxt
        , _secondarySkillInputDisplayText    = secondarySkillInputDisplayTxt
        , _secondarySkillOverlayImage        = secondarySkillOverlayImg
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
            ssInfoTexts = SecondarySkillInfoTexts
                { _neutral = neutralDisplayTxt
                , _up      = upDisplayTxt
                , _down    = downDisplayTxt
                }
        return $ SecondarySkillInfoTextType secondarySkillInfoTextAliveSecs equipmentInfoInputDisplayTxt ssInfoTexts

    UiMsgShowGeneralEquipmentInfo -> return $
        EquipmentInfoTextType equipmentInfoTextAliveSecs equipmentInfoInputDisplayTxt Nothing

    _ -> processUiMsgs infoTextUI ps

    where equipmentInfoInputDisplayTxt = _equipmentInfoInputDisplayText infoTextUI

updateInfoTextUI
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsRead UpdateWorldUiMsgsPhase m)
    => InfoTextUI
    -> m InfoTextUI
updateInfoTextUI infoTextUI = do
    infoTextType  <- processUiMsgs infoTextUI =<< readMsgs
    infoTextType' <- updateInfoTextType infoTextType

    moveControlsLeftInputDisplayTxt  <- updateInputDisplayText $ _moveControlsLeftInputDisplayText infoTextUI
    moveControlsRightInputDisplayTxt <- updateInputDisplayText $ _moveControlsRightInputDisplayText infoTextUI
    equipmentInfoInputDisplayTxt     <- updateInputDisplayText $ _equipmentInfoInputDisplayText infoTextUI
    switchWeaponInputDisplayTxt      <- updateInputDisplayText $ _switchWeaponInputDisplayText infoTextUI
    switchGunInputDisplayTxt         <- updateInputDisplayText $ _switchGunInputDisplayText infoTextUI
    secondarySkillInputDisplayTxt    <- updateInputDisplayText $ _secondarySkillInputDisplayText infoTextUI

    return $ infoTextUI
        { _infoTextType                      = infoTextType'
        , _moveControlsLeftInputDisplayText  = moveControlsLeftInputDisplayTxt
        , _moveControlsRightInputDisplayText = moveControlsRightInputDisplayTxt
        , _equipmentInfoInputDisplayText     = equipmentInfoInputDisplayTxt
        , _switchWeaponInputDisplayText      = switchWeaponInputDisplayTxt
        , _switchGunInputDisplayText         = switchGunInputDisplayTxt
        , _secondarySkillInputDisplayText    = secondarySkillInputDisplayTxt
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

drawSecondarySkillInfo
    :: (GraphicsReadWrite m, InputRead m, MonadIO m)
    => InputDisplayText
    -> SecondarySkillInfoTexts
    -> InfoTextUI
    -> m ()
drawSecondarySkillInfo text3InputDisplayTxt secondarySkillInfoTxts infoTextUI = do
    let
        img                = _secondarySkillOverlayImage infoTextUI
        imgWidth           = imageWidth img
        symbolLeftPadWidth = imgWidth + secondarySkillTextSpacerWidth

    symbolWidths <- map (symbolLeftPadWidth +) <$> sequenceA
        [ displayTextWidth $ _neutral secondarySkillInfoTxts
        , displayTextWidth $ _up secondarySkillInfoTxts
        , displayTextWidth $ _down secondarySkillInfoTxts
        ]
    widths       <- (:symbolWidths) <$> inputDisplayTextWidth text3InputDisplayTxt

    let
        rectWidth   = fromMaybe 0.0 (maybeMaximum widths) + textBackdropBorderSize * 2.0
        textCenterX = virtualRenderWidth / 2.0
        rectX       = textCenterX - rectWidth / 2.0
        rectPos     = Pos2 rectX secondarySkillInfoBackdropPosY
    drawRect rectPos rectWidth secondarySkillInfoBackdropHeight textBackdropColor uiInfoTextZIndex

    let
        symbolMaxWidth    = fromMaybe 0.0 (maybeMaximum symbolWidths)
        imgLeft           = rectX + (rectWidth - symbolMaxWidth) / 2.0
        imgPos            = Pos2 imgLeft secondarySkillInfoImagePosY
        imgRight          = imgLeft + imgWidth + secondarySkillTextSpacerWidth
        rectCenterX       = rectX + rectWidth / 2.0
        text0Pos          = Pos2 imgRight secondarySkillInfoText0PosY
        text1Pos          = Pos2 imgRight secondarySkillInfoText1PosY
        text2Pos          = Pos2 imgRight secondarySkillInfoText2PosY
        text3Pos          = Pos2 rectCenterX secondarySkillInfoText3PosY
        neutralDisplayTxt = _neutral secondarySkillInfoTxts
        upDisplayTxt      = _up secondarySkillInfoTxts
        downDisplayTxt    = _down secondarySkillInfoTxts
    drawImage imgPos RightDir uiInfoTextZIndex img
    drawDisplayText text0Pos uiInfoTextZIndex neutralDisplayTxt
    drawDisplayText text1Pos uiInfoTextZIndex upDisplayTxt
    drawDisplayText text2Pos uiInfoTextZIndex downDisplayTxt
    drawInputDisplayTextCentered text3Pos uiInfoTextZIndex text3InputDisplayTxt

    when (_text (neutralDisplayTxt :: DisplayText) == secondarySkillColonText) $
        let emptyText0Pos = text0Pos `vecAdd` secondarySkillEmptyTextOffset
        in drawDisplayText emptyText0Pos uiInfoTextZIndex (_secondarySkillLiteralEmptyText infoTextUI)
    when (_text (upDisplayTxt :: DisplayText) == secondarySkillColonText) $
        let emptyText1Pos = text1Pos `vecAdd` secondarySkillEmptyTextOffset
        in drawDisplayText emptyText1Pos uiInfoTextZIndex (_secondarySkillLiteralEmptyText infoTextUI)
    when (_text (downDisplayTxt :: DisplayText) == secondarySkillColonText) $
        let emptyText2Pos = text2Pos `vecAdd` secondarySkillEmptyTextOffset
        in drawDisplayText emptyText2Pos uiInfoTextZIndex (_secondarySkillLiteralEmptyText infoTextUI)

    let
        secondarySkillSymbolDisplayTxt      = _secondarySkillSymbolDisplayText infoTextUI
        secondarySkillSymbolDisplayTxtWidth = symbolDisplayTextImageWidth secondarySkillSymbolDisplayTxt
        secondarySkillInputDisplayTxt       = _secondarySkillInputDisplayText infoTextUI
    secondarySkillInputDisplayTxtWidth <- inputDisplayTextWidth secondarySkillInputDisplayTxt
    let
        symbolWidth     = secondarySkillSymbolDisplayTxtWidth + secondarySkillInputDisplayTxtWidth
        symbolRectWidth = symbolWidth + textBackdropBorderSize * 2.0
        symbolRectX     = rectX + rectWidth + secondarySkillSymbolSpacerWidth
        symbolRectPos   = Pos2 symbolRectX secondarySkillSymbolBackdropPosY
    drawRect symbolRectPos symbolRectWidth secondarySkillSymbolBackdropHeight textBackdropColor uiInfoTextZIndex

    let
        symbolTextX   = symbolRectX + textBackdropBorderSize
        symbolTextPos = Pos2 symbolTextX secondarySkillSymbolText0PosY
        inputTextPos  = Pos2 (symbolTextX + secondarySkillSymbolDisplayTxtWidth) secondarySkillSymbolText0PosY
    drawSymbolDisplayText symbolTextPos uiInfoTextZIndex secondarySkillSymbolDisplayTxt
    drawInputDisplayText inputTextPos uiInfoTextZIndex secondarySkillInputDisplayTxt

drawInfoTextUI :: (GraphicsReadWrite m, InputRead m, MonadIO m) => InfoTextUI -> m ()
drawInfoTextUI infoTextUI = case _infoTextType infoTextUI of
    NoInfoTextType -> return ()

    MoveControlsInfoTextType leftInputDisplayTxt rightInputDisplayTxt ->
        drawMoveControlsInfo leftInputDisplayTxt rightInputDisplayTxt

    EquipmentInfoTextType _ line0InputDisplayTxt line1InputDisplayTxt ->
        drawEquipmentInfo line0InputDisplayTxt line1InputDisplayTxt

    SecondarySkillInfoTextType _ text3InputDisplayTxt secondarySkillInfoTxts -> do
        drawSecondarySkillInfo text3InputDisplayTxt secondarySkillInfoTxts infoTextUI

resetInfoTextUIOnChangeWorldRoom :: InfoTextUI -> InfoTextUI
resetInfoTextUIOnChangeWorldRoom infoTextUI = infoTextUI {_infoTextType = NoInfoTextType}
