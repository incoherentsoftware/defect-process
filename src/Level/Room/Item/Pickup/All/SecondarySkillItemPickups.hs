module Level.Room.Item.Pickup.All.SecondarySkillItemPickups
    ( mkStoneFormItemPickup
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))
import Data.Maybe             (fromMaybe, isNothing)
import qualified Data.Text as T

import AppEnv
import Collision.Hitbox
import Configs
import Configs.All.Level
import FileCache
import Level.Room.Item as RI
import Level.Room.Item.Pickup
import Level.Room.Item.Pickup.Util
import Level.Room.Types
import Msg
import Player.EquipmentInfo
import Player.SecondarySkill.All
import Player.SecondarySkill.Types as SS
import Util
import Window.Graphics
import Window.InputState
import World.Util
import World.ZIndex

-- NOTE: this is modified from the full source since only stoneForm is included in this repo
stoneFormImgFileName = "stone-form-pickup.image" :: FileName

secondarySkillInfoBackdropOffsetY = -366.0        :: OffsetY
secondarySkillInfoImageOffsetY    = -316.0        :: OffsetY
assignTextOffsetY                 = -339.0        :: OffsetY
secondarySkillInfoText0OffsetY    = -308.0        :: OffsetY
secondarySkillInfoText1OffsetY    = -251.0        :: OffsetY
secondarySkillInfoText2OffsetY    = -196.0        :: OffsetY
literalEmptyTextOffset            = Pos2 20.0 0.0 :: Pos2
secondarySkillInfoBackdropHeight  = 221.0         :: Float
replaceTextOffsetY                = -374.0        :: OffsetY
replaceRelativeOffsetY            = -33.0         :: OffsetY

assignText     = "Assign Input" :: T.Text
slotsColonText = " :"           :: T.Text

selectedLineIndexToSecondarySkillSlot :: Int -> SecondarySkillSlot
selectedLineIndexToSecondarySkillSlot = \case
    2 -> SecondarySkillUpSlot
    3 -> SecondarySkillDownSlot
    _ -> SecondarySkillNeutralSlot

secondarySkillSlotToSelectedLineIndex :: SecondarySkillSlot -> Int
secondarySkillSlotToSelectedLineIndex = \case
    SecondarySkillNeutralSlot -> 1
    SecondarySkillUpSlot      -> 2
    SecondarySkillDownSlot    -> 3

buyConfirmStartMessages :: MsgsRead ThinkLevelMsgsPhase m => ItemPickupBuyConfirmStartMessages m
buyConfirmStartMessages item = do
    playerEquipment <- readPlayerEquipmentInfo
    let
        selectedLineIndex = secondarySkillSlotToSelectedLineIndex $ if
            | isNothing (_secondarySkillNeutralType playerEquipment) -> SecondarySkillNeutralSlot
            | isNothing (_secondarySkillUpType playerEquipment)      -> SecondarySkillUpSlot
            | isNothing (_secondarySkillDownType playerEquipment)    -> SecondarySkillDownSlot
            | otherwise                                              -> SecondarySkillNeutralSlot

        update = \i ->
            let buyConfirmData = _buyConfirmData $ RI._data i
            in i
                { RI._data = (RI._data i)
                    { _buyConfirmData = buyConfirmData
                        { _selectedLineIndex = selectedLineIndex
                        , _line0DisplayText  = updateDisplayText assignText (_line0DisplayText buyConfirmData)
                        }
                    , _status         = ItemPickupBuyConfirmStatus
                    }
                }
    return [mkMsgTo (RoomMsgUpdateItem update) (RI._msgId item)]

calculateSecondarySkillSlots
    :: (AllowMsgRead p InfoMsgPayload, MsgsRead p m)
    => ItemPickupData
    -> m (Maybe SecondarySkillType, Maybe SecondarySkillType, Maybe SecondarySkillType, Maybe SecondarySkillType)
calculateSecondarySkillSlots itemData = calc <$> readPlayerEquipmentInfo
    where
        calc
            :: PlayerEquipmentInfo
            -> (Maybe SecondarySkillType, Maybe SecondarySkillType, Maybe SecondarySkillType, Maybe SecondarySkillType)
        calc playerEquipment = case selectedSlot of
            SecondarySkillNeutralSlot -> case (neutralSkillType, upSkillType, downSkillType) of
                (Nothing, _, _)          -> (buySkillType, upSkillType, downSkillType, Nothing)
                (Just _, Nothing, _)     -> (buySkillType, neutralSkillType, downSkillType, Nothing)
                (Just _, _, Nothing)     -> (buySkillType, neutralSkillType, upSkillType, Nothing)
                (Just _, Just _, Just _) -> (buySkillType, upSkillType, downSkillType, neutralSkillType)
            SecondarySkillUpSlot      -> case (neutralSkillType, upSkillType, downSkillType) of
                (_, Nothing, _)          -> (neutralSkillType, buySkillType, downSkillType, Nothing)
                (Nothing, Just _, _)     -> (upSkillType, buySkillType, downSkillType, Nothing)
                (_, Just _, Nothing)     -> (neutralSkillType, buySkillType, upSkillType, Nothing)
                (Just _, Just _, Just _) -> (neutralSkillType, buySkillType, downSkillType, upSkillType)
            SecondarySkillDownSlot    -> case (neutralSkillType, upSkillType, downSkillType) of
                (_, _, Nothing)          -> (neutralSkillType, upSkillType, buySkillType, Nothing)
                (_, Nothing, Just _)     -> (neutralSkillType, downSkillType, buySkillType, Nothing)
                (Nothing, _, Just _)     -> (upSkillType, downSkillType, buySkillType, Nothing)
                (Just _, Just _, Just _) -> (neutralSkillType, upSkillType, buySkillType, downSkillType)
            where
                neutralSkillType = _secondarySkillNeutralType playerEquipment
                upSkillType      = _secondarySkillUpType playerEquipment
                downSkillType    = _secondarySkillDownType playerEquipment

                selectedSlot = selectedLineIndexToSecondarySkillSlot $ _selectedLineIndex (_buyConfirmData itemData)
                buySkillType = case _buyMsgPayload itemData of
                    PlayerMsgBuySecondarySkill (Some skill) _ -> Just $ SS._type skill
                    _                                         -> Nothing

thinkBuyConfirm :: (InputRead m, MsgsRead ThinkLevelMsgsPhase m) => ItemPickupThinkBuyConfirm m
thinkBuyConfirm item = readInputState >>= \inputState -> if
    | InteractAlias `aliasPressed` inputState -> do
        (neutralSkillType, upSkillType, downSkillType, replacedSkillType) <-
            calculateSecondarySkillSlots $ RI._data item

        let
            removeReplacedSkillMsgs = case replacedSkillType of
                Just skillType -> [mkMsgEx (PlayerMsgClearSecondarySkill skillType) MsgFrontOrder]
                Nothing        -> []
            setSkillSlotsMsg        =
                mkMsgEx (PlayerMsgSetSecondarySkillSlots neutralSkillType upSkillType downSkillType) MsgEndOrder
        buyMsgs <- itemPickupBuyMessages item
        return $ buyMsgs ++ removeReplacedSkillMsgs ++ [setSkillSlotsMsg]

    | otherwise -> return []

updateBuyConfirm
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsRead UpdateLevelMsgsPhase m)
    => ItemPickupUpdateBuyConfirm m
updateBuyConfirm itemData = do
    inputState <- readInputState
    let
        upPressed      = UpAlias `aliasPressed` inputState
        downPressed    = DownAlias `aliasPressed` inputState
        status         = _status itemData
        buyConfirmData = _buyConfirmData itemData
        selectedSlot   = selectedLineIndexToSecondarySkillSlot $ _selectedLineIndex buyConfirmData

        selectedSlot'
            | status /= ItemPickupBuyConfirmStatus = selectedSlot
            | otherwise                            = case selectedSlot of
                SecondarySkillNeutralSlot
                    | downPressed -> SecondarySkillUpSlot
                SecondarySkillUpSlot
                    | upPressed   -> SecondarySkillNeutralSlot
                    | downPressed -> SecondarySkillDownSlot
                SecondarySkillDownSlot
                    | upPressed   -> SecondarySkillUpSlot
                _                 -> selectedSlot

    upAliasInputDisplayTxt       <- updateInputDisplayText $ _upAliasInputDisplayText buyConfirmData
    downAliasInputDisplayTxt     <- updateInputDisplayText $ _downAliasInputDisplayText buyConfirmData
    interactAliasInputDisplayTxt <- updateInputDisplayText $ _interactAliasInputDisplayText buyConfirmData

    (neutralSkillType, upSkillType, downSkillType, replacedType) <- calculateSecondarySkillSlots $ itemData
        { _buyConfirmData = buyConfirmData {_selectedLineIndex = secondarySkillSlotToSelectedLineIndex selectedSlot'}
        }
    let
        formatSecondarySkillText = \case
            Just t  -> " : " <> prettyShow t
            Nothing -> slotsColonText
        formatReplaceText        = \case
            Just t  -> "(Replace " <> prettyShow t <> ")"
            Nothing -> ""

        neutralTxt  = formatSecondarySkillText neutralSkillType
        upTxt       = formatSecondarySkillText upSkillType
        downTxt     = formatSecondarySkillText downSkillType
        replacedTxt = formatSecondarySkillText replacedType
        replaceTxt  = formatReplaceText replacedType

    return $ buyConfirmData
        { _selectedLineIndex             = secondarySkillSlotToSelectedLineIndex selectedSlot'
        , _upAliasInputDisplayText       = upAliasInputDisplayTxt
        , _downAliasInputDisplayText     = downAliasInputDisplayTxt
        , _interactAliasInputDisplayText = interactAliasInputDisplayTxt
        , _replace0DisplayText           = updateDisplayText replaceTxt (_replace0DisplayText buyConfirmData)
        , _line1DisplayText              = updateDisplayText neutralTxt (_line1DisplayText buyConfirmData)
        , _line1SelectedDisplayText      = updateDisplayText neutralTxt (_line1SelectedDisplayText buyConfirmData)
        , _line2DisplayText              = updateDisplayText upTxt (_line2DisplayText buyConfirmData)
        , _line2SelectedDisplayText      = updateDisplayText upTxt (_line2SelectedDisplayText buyConfirmData)
        , _line3DisplayText              = updateDisplayText downTxt (_line3DisplayText buyConfirmData)
        , _line3SelectedDisplayText      = updateDisplayText downTxt (_line3SelectedDisplayText buyConfirmData)
        , _line4DisplayText              = updateDisplayText replacedTxt (_line4DisplayText buyConfirmData)
        }

drawBuyConfirmOverlay :: (GraphicsReadWrite m, InputRead m, MonadIO m) => ItemPickupDrawBuyConfirm m
drawBuyConfirmOverlay item = do
    let
        buyConfirmData    = _buyConfirmData $ RI._data item
        replaceDisplayTxt = _replace0DisplayText buyConfirmData
        isShowReplace     = _text (replaceDisplayTxt :: DisplayText) /= ""

        selectedSlot = selectedLineIndexToSecondarySkillSlot $ _selectedLineIndex buyConfirmData
        img          = case selectedSlot of
            SecondarySkillNeutralSlot -> _slotsOverlayNeutralSelectedImage buyConfirmData
            SecondarySkillUpSlot      -> _slotsOverlayUpSelectedImage buyConfirmData
            SecondarySkillDownSlot    -> _slotsOverlayDownSelectedImage buyConfirmData
        imgWidth     = imageWidth img

    symbolWidths <- map (imgWidth +) <$> sequenceA
        [ displayTextWidth $ _line1DisplayText buyConfirmData
        , displayTextWidth $ _line2DisplayText buyConfirmData
        , displayTextWidth $ _line3DisplayText buyConfirmData
        , displayTextWidth $ _line4DisplayText buyConfirmData
        ]

    let
        Pos2 itemX itemY    = hitboxCenter $ RI._hitbox item
        rectWidth           = fromMaybe 0.0 (maybeMaximum symbolWidths) + itemPickupOverlayBackdropBorderSize * 2.0
        rectX               = itemX - rectWidth / 2.0
        rectY
            | isShowReplace = itemY + secondarySkillInfoBackdropOffsetY + replaceRelativeOffsetY
            | otherwise     = itemY + secondarySkillInfoBackdropOffsetY
        rectPos             = Pos2 rectX rectY
        rectHeight
            | isShowReplace = secondarySkillInfoBackdropHeight - replaceRelativeOffsetY
            | otherwise     = secondarySkillInfoBackdropHeight
    drawRect rectPos rectWidth rectHeight itemPickupOverlayBackdropColor uiInfoTextZIndex

    let
        symbolMaxWidth = fromMaybe 0.0 (maybeMaximum symbolWidths)
        imgLeft        = fromIntegral $ round (rectX + (rectWidth - symbolMaxWidth) / 2.0)
        imgPos         = Pos2 imgLeft (itemY + secondarySkillInfoImageOffsetY)
        imgRight       = imgLeft + imgWidth
        rectCenterX    = rectX + rectWidth / 2.0
        assignPos      = Pos2 rectCenterX (itemY + assignTextOffsetY)
        text0Pos       = Pos2 imgRight (itemY + secondarySkillInfoText0OffsetY)
        text1Pos       = Pos2 imgRight (itemY + secondarySkillInfoText1OffsetY)
        text2Pos       = Pos2 imgRight (itemY + secondarySkillInfoText2OffsetY)

        neutralDisplayTxt = case selectedSlot of
            SecondarySkillNeutralSlot -> _line1SelectedDisplayText buyConfirmData
            _                         -> _line1DisplayText buyConfirmData
        upDisplayTxt      = case selectedSlot of
            SecondarySkillUpSlot -> _line2SelectedDisplayText buyConfirmData
            _                    -> _line2DisplayText buyConfirmData
        downDisplayTxt    = case selectedSlot of
            SecondarySkillDownSlot -> _line3SelectedDisplayText buyConfirmData
            _                      -> _line3DisplayText buyConfirmData

    when isShowReplace $
        let replacePos = Pos2 rectCenterX (itemY + replaceTextOffsetY)
        in drawDisplayTextCentered replacePos uiInfoTextZIndex replaceDisplayTxt

    drawImage imgPos RightDir uiInfoTextZIndex img
    drawDisplayTextCentered assignPos uiInfoTextZIndex (_line0DisplayText buyConfirmData)
    drawDisplayText text0Pos uiInfoTextZIndex neutralDisplayTxt
    drawDisplayText text1Pos uiInfoTextZIndex upDisplayTxt
    drawDisplayText text2Pos uiInfoTextZIndex downDisplayTxt

    when (_text (neutralDisplayTxt :: DisplayText) == slotsColonText) $
        let emptyText0Pos = text0Pos `vecAdd` literalEmptyTextOffset
        in drawDisplayText emptyText0Pos uiInfoTextZIndex (_literalEmptyText buyConfirmData)
    when (_text (upDisplayTxt :: DisplayText) == slotsColonText) $
        let emptyText1Pos = text1Pos `vecAdd` literalEmptyTextOffset
        in drawDisplayText emptyText1Pos uiInfoTextZIndex (_literalEmptyText buyConfirmData)
    when (_text (downDisplayTxt :: DisplayText) == slotsColonText) $
        let emptyText2Pos = text2Pos `vecAdd` literalEmptyTextOffset
        in drawDisplayText emptyText2Pos uiInfoTextZIndex (_literalEmptyText buyConfirmData)

    drawItemPickupBuyConfirmControlsOverlay (rectX + rectWidth) item

mkSecondarySkillItemPickupData
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => SecondarySkillType
    -> PlayerMsgPayload
    -> GoldValue
    -> FilePath
    -> RoomType
    -> m ItemPickupData
mkSecondarySkillItemPickupData typ buyMsgPayload cost imgFileName roomType =
    mkItemPickupData typ buyMsgPayload cost imgFileName roomType <&> \itemData -> itemData
        { _isBuyConfirmOnInteract  = return True
        , _buyConfirmStartMessages = buyConfirmStartMessages
        , _thinkBuyConfirm         = thinkBuyConfirm
        , _updateBuyConfirm        = updateBuyConfirm
        , _drawBuyConfirmOverlay   = drawBuyConfirmOverlay
        }

mkSecondarySkillItemPickup :: MonadIO m => Pos2 -> ItemPickupData -> m (Some RoomItem)
mkSecondarySkillItemPickup pos itemData = mkItemPickup pos SecondarySkillPickupItemType itemData

mkStoneFormItemPickup :: RoomType -> Pos2 -> AppEnv p (Some RoomItem)
mkStoneFormItemPickup roomType pos = do
    stoneFormCost <- readConfig _level _itemPickupStoneFormSkillGoldValue
    buyMsgPayload <- PlayerMsgBuySecondarySkill <$> mkStoneFormSkill <*> pure stoneFormCost
    itemData      <-
        mkSecondarySkillItemPickupData StoneFormSkill buyMsgPayload stoneFormCost stoneFormImgFileName roomType
    mkSecondarySkillItemPickup pos itemData
