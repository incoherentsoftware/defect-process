module Level.Room.Item.Pickup.All.GunItemPickups
    ( module Player.Gun.All
    , mkRevolverItemPickup
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))
import qualified Data.List.NonEmpty as NE

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
import Player.Gun.All
import Player.Gun.Types as G
import Util
import Window.Graphics
import Window.InputState
import World.Util
import World.ZIndex

-- NOTE: this is modified from the full source since only revolver is included in this repo
revolverPickupImgFileName = "revolver-pickup.image" :: FileName

gunInfoBackdropOffsetY = -340.0 :: OffsetY
replaceTextOffsetY     = -293.0 :: OffsetY
gunLine1TextOffsetY    = -262.0 :: OffsetY
gunLine2TextOffsetY    = -220.0 :: OffsetY
gunInfoBackdropHeight  = 195.0  :: Float

buyConfirmStartMessages :: Monad m => ItemPickupBuyConfirmStartMessages m
buyConfirmStartMessages item = return [mkMsgTo (RoomMsgUpdateItem update) (RI._msgId item)]
    where
        update = \i ->
            let buyConfirmData = _buyConfirmData $ RI._data i
            in i
                { RI._data = (RI._data i)
                    { _buyConfirmData = buyConfirmData {_selectedLineIndex = 1}
                    , _status         = ItemPickupBuyConfirmStatus
                    }
                }

thinkBuyConfirm :: (InputRead m, MsgsRead ThinkLevelMsgsPhase m) => ItemPickupThinkBuyConfirm m
thinkBuyConfirm item = readInputState >>= \inputState -> if
    | InteractAlias `aliasPressed` inputState -> do
        (_, _, replacedGunType) <- calculateGunSlots $ RI._data item
        let clearGunMsgs         = maybe [] (\t -> [mkMsgEx (PlayerMsgClearGun t) MsgFrontOrder]) replacedGunType
        (clearGunMsgs ++) <$> itemPickupBuyMessages item
    | otherwise                               -> return []

calculateGunSlots
    :: (AllowMsgRead p InfoMsgPayload, MsgsRead p m)
    => ItemPickupData
    -> m (Maybe GunType, Maybe GunType, Maybe GunType)
calculateGunSlots itemData = calc <$> readPlayerEquipmentInfo
    where
        calc :: PlayerEquipmentInfo -> (Maybe GunType, Maybe GunType, Maybe GunType)
        calc playerEquipment = case selectedLineIndex of
            2 -> (gunTypes !!? 0, buyGunType, gunTypes !!? 1)
            _ -> (buyGunType, gunTypes !!? 1, gunTypes !!? 0)
            where
                selectedLineIndex = _selectedLineIndex $ _buyConfirmData itemData
                gunTypes          = _gunTypes playerEquipment
                buyGunType        = case _buyMsgPayload itemData of
                    PlayerMsgBuyGun (Some gun) _ -> Just $ G._type gun
                    _                            -> Nothing

updateBuyConfirm
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsRead UpdateLevelMsgsPhase m)
    => ItemPickupUpdateBuyConfirm m
updateBuyConfirm itemData = do
    inputState <- readInputState
    let
        upPressed         = UpAlias `aliasPressed` inputState
        downPressed       = DownAlias `aliasPressed` inputState
        status            = _status itemData
        buyConfirmData    = _buyConfirmData itemData
        selectedLineIndex = _selectedLineIndex buyConfirmData

        selectedLineIndex'
            | status /= ItemPickupBuyConfirmStatus  = selectedLineIndex
            | selectedLineIndex == 1 && downPressed = 2
            | selectedLineIndex == 2 && upPressed   = 1
            | otherwise                             = selectedLineIndex

    upAliasInputDisplayTxt       <- updateInputDisplayText $ _upAliasInputDisplayText buyConfirmData
    downAliasInputDisplayTxt     <- updateInputDisplayText $ _downAliasInputDisplayText buyConfirmData
    interactAliasInputDisplayTxt <- updateInputDisplayText $ _interactAliasInputDisplayText buyConfirmData

    (line1GunType, line2GunType, replacedGunType) <- calculateGunSlots itemData
    let
        formatGunText     = \t -> maybe "" prettyShow t
        formatReplaceText = \t -> "(Replace " <> formatGunText t <> ")"
        gunLine1Txt       = "• " <> formatGunText line1GunType
        gunLine2Txt       = "• " <> formatGunText line2GunType
        gunLine3Txt       = "• " <> formatGunText replacedGunType
        replace0Txt       = formatReplaceText replacedGunType
        replace1Txt       = formatReplaceText line1GunType
        replace2Txt       = formatReplaceText line2GunType

    return $ buyConfirmData
        { _selectedLineIndex             = selectedLineIndex'
        , _upAliasInputDisplayText       = upAliasInputDisplayTxt
        , _downAliasInputDisplayText     = downAliasInputDisplayTxt
        , _interactAliasInputDisplayText = interactAliasInputDisplayTxt
        , _replace0DisplayText           = updateDisplayText replace0Txt (_replace0DisplayText buyConfirmData)
        , _replace1DisplayText           = updateDisplayText replace1Txt (_replace1DisplayText buyConfirmData)
        , _replace2DisplayText           = updateDisplayText replace2Txt (_replace2DisplayText buyConfirmData)
        , _line1DisplayText              = updateDisplayText gunLine1Txt (_line1DisplayText buyConfirmData)
        , _line1SelectedDisplayText      = updateDisplayText gunLine1Txt (_line1SelectedDisplayText buyConfirmData)
        , _line2DisplayText              = updateDisplayText gunLine2Txt (_line2DisplayText buyConfirmData)
        , _line2SelectedDisplayText      = updateDisplayText gunLine2Txt (_line2SelectedDisplayText buyConfirmData)
        , _line3DisplayText              = updateDisplayText gunLine3Txt (_line3DisplayText buyConfirmData)
        }

drawBuyConfirmOverlay :: (GraphicsReadWrite m, InputRead m, MonadIO m) => ItemPickupDrawBuyConfirm m
drawBuyConfirmOverlay item = do
    let buyConfirmData = _buyConfirmData $ RI._data item

    gunLinesMaxWidth <- fmap maximum . sequenceA $ NE.fromList
        [ displayTextWidth $ _line1DisplayText buyConfirmData
        , displayTextWidth $ _line2DisplayText buyConfirmData
        , displayTextWidth $ _line3DisplayText buyConfirmData
        ]
    replaceMaxWidth  <- fmap maximum . sequenceA $ NE.fromList
        [ displayTextWidth $ _replace0DisplayText buyConfirmData
        , displayTextWidth $ _replace1DisplayText buyConfirmData
        , displayTextWidth $ _replace2DisplayText buyConfirmData
        ]

    let
        Pos2 itemX itemY = hitboxCenter $ RI._hitbox item
        allLinesMaxWidth = max gunLinesMaxWidth replaceMaxWidth
        rectWidth        = allLinesMaxWidth + itemPickupOverlayBackdropBorderSize * 2.0
        rectX            = itemX - rectWidth / 2.0
        rectPos          = Pos2 rectX (itemY + gunInfoBackdropOffsetY)
    drawRect rectPos rectWidth gunInfoBackdropHeight itemPickupOverlayBackdropColor uiInfoTextZIndex

    let
        rectCenterX = rectX + rectWidth / 2.0
        replacePos  = Pos2 rectCenterX (itemY + replaceTextOffsetY)
        gunLineX    = rectCenterX - gunLinesMaxWidth / 2.0
        text1Pos    = Pos2 gunLineX (itemY + gunLine1TextOffsetY)
        text2Pos    = Pos2 gunLineX (itemY + gunLine2TextOffsetY)

        selectedLineIndex = _selectedLineIndex buyConfirmData
        line1DisplayTxt   = case selectedLineIndex of
            1 -> _line1SelectedDisplayText buyConfirmData
            _ -> _line1DisplayText buyConfirmData
        line2DisplayTxt   = case selectedLineIndex of
            2 -> _line2SelectedDisplayText buyConfirmData
            _ -> _line2DisplayText buyConfirmData

    drawDisplayTextCentered replacePos uiInfoTextZIndex (_replace0DisplayText buyConfirmData)
    drawDisplayText text1Pos uiInfoTextZIndex line1DisplayTxt
    drawDisplayText text2Pos uiInfoTextZIndex line2DisplayTxt

    drawItemPickupBuyConfirmControlsOverlay (rectX + rectWidth) item

isBuyConfirmOnInteract :: MsgsRead ThinkLevelMsgsPhase m => ItemPickupIsBuyConfirmOnInteract m
isBuyConfirmOnInteract = isPlayerEquipmentInfoGunsFull <$> readPlayerEquipmentInfo

mkGunItemPickupData
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => GunType
    -> PlayerMsgPayload
    -> GoldValue
    -> FilePath
    -> RoomType
    -> m ItemPickupData
mkGunItemPickupData typ buyMsgPayload cost imgFileName roomType =
    mkItemPickupData typ buyMsgPayload cost imgFileName roomType <&> \itemData -> itemData
        { _isBuyConfirmOnInteract  = isBuyConfirmOnInteract
        , _buyConfirmStartMessages = buyConfirmStartMessages
        , _thinkBuyConfirm         = thinkBuyConfirm
        , _updateBuyConfirm        = updateBuyConfirm
        , _drawBuyConfirmOverlay   = drawBuyConfirmOverlay
        }

mkGunItemPickup :: MonadIO m => Pos2 -> ItemPickupData -> m (Some RoomItem)
mkGunItemPickup pos itemData = mkItemPickup pos GunPickupItemType itemData

mkRevolverItemPickup :: RoomType -> Pos2 -> AppEnv p (Some RoomItem)
mkRevolverItemPickup roomType pos = do
    revolverCost   <- readConfig _level _itemPickupGunGoldValue
    buyMsgPayload  <- PlayerMsgBuyGun <$> mkRevolverGun <*> pure revolverCost
    itemPickupData <- mkGunItemPickupData RevolverGun buyMsgPayload revolverCost revolverPickupImgFileName roomType
    mkGunItemPickup pos itemPickupData
