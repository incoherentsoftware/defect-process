module Level.Room.Item.Pickup.All.WeaponItemPickups
    ( mkSwordItemPickup
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
import Player.Weapon as W
import Player.Weapon.All
import Util
import Window.Graphics
import Window.InputState
import World.Util
import World.ZIndex

-- NOTE: this is modified from the full source since only sword is included in this repo
swordPickupImgFileName = "sword-pickup.image" :: FileName

wpnInfoBackdropOffsetY = -340.0 :: OffsetY
replaceTextOffsetY     = -293.0 :: OffsetY
wpnLine1TextOffsetY    = -262.0 :: OffsetY
wpnLine2TextOffsetY    = -220.0 :: OffsetY
wpnInfoBackdropHeight  = 195.0  :: Float

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
        (_, _, replacedWpnType) <- calculateWeaponSlots $ RI._data item
        let clearWeaponMsgs      = maybe [] (\t -> [mkMsgEx (PlayerMsgClearWeapon t) MsgFrontOrder]) replacedWpnType
        (clearWeaponMsgs ++) <$> itemPickupBuyMessages item
    | otherwise                               -> return []

calculateWeaponSlots
    :: (AllowMsgRead p InfoMsgPayload, MsgsRead p m)
    => ItemPickupData
    -> m (Maybe WeaponType, Maybe WeaponType, Maybe WeaponType)
calculateWeaponSlots itemData = calc <$> readPlayerEquipmentInfo
    where
        calc :: PlayerEquipmentInfo -> (Maybe WeaponType, Maybe WeaponType, Maybe WeaponType)
        calc playerEquipment = case selectedLineIndex of
            2 -> (wpnTypes !!? 0, buyWpnType, wpnTypes !!? 1)
            _ -> (buyWpnType, wpnTypes !!? 1, wpnTypes !!? 0)
            where
                selectedLineIndex = _selectedLineIndex $ _buyConfirmData itemData
                wpnTypes          = _weaponTypes playerEquipment
                buyWpnType        = case _buyMsgPayload itemData of
                    PlayerMsgBuyWeapon (Some wpn) _ -> Just $ W._type wpn
                    _                               -> Nothing

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

    (line1WpnType, line2WpnType, replacedWpnType) <- calculateWeaponSlots itemData
    let
        formatWeaponText  = \t -> maybe "" prettyShow t
        formatReplaceText = \t -> "(Replace " <> formatWeaponText t <> ")"
        wpnLine1Txt       = "• " <> formatWeaponText line1WpnType
        wpnLine2Txt       = "• " <> formatWeaponText line2WpnType
        wpnLine3Txt       = "• " <> formatWeaponText replacedWpnType
        replace0Txt       = formatReplaceText replacedWpnType
        replace1Txt       = formatReplaceText line1WpnType
        replace2Txt       = formatReplaceText line2WpnType

    return $ buyConfirmData
        { _selectedLineIndex             = selectedLineIndex'
        , _upAliasInputDisplayText       = upAliasInputDisplayTxt
        , _downAliasInputDisplayText     = downAliasInputDisplayTxt
        , _interactAliasInputDisplayText = interactAliasInputDisplayTxt
        , _replace0DisplayText           = updateDisplayText replace0Txt (_replace0DisplayText buyConfirmData)
        , _replace1DisplayText           = updateDisplayText replace1Txt (_replace1DisplayText buyConfirmData)
        , _replace2DisplayText           = updateDisplayText replace2Txt (_replace2DisplayText buyConfirmData)
        , _line1DisplayText              = updateDisplayText wpnLine1Txt (_line1DisplayText buyConfirmData)
        , _line1SelectedDisplayText      = updateDisplayText wpnLine1Txt (_line1SelectedDisplayText buyConfirmData)
        , _line2DisplayText              = updateDisplayText wpnLine2Txt (_line2DisplayText buyConfirmData)
        , _line2SelectedDisplayText      = updateDisplayText wpnLine2Txt (_line2SelectedDisplayText buyConfirmData)
        , _line3DisplayText              = updateDisplayText wpnLine3Txt (_line3DisplayText buyConfirmData)
        }

drawBuyConfirmOverlay :: (GraphicsReadWrite m, InputRead m, MonadIO m) => ItemPickupDrawBuyConfirm m
drawBuyConfirmOverlay item = do
    let buyConfirmData = _buyConfirmData $ RI._data item

    wpnLinesMaxWidth <- fmap maximum . sequenceA $ NE.fromList
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
        allLinesMaxWidth = max wpnLinesMaxWidth replaceMaxWidth
        rectWidth        = allLinesMaxWidth + itemPickupOverlayBackdropBorderSize * 2.0
        rectX            = itemX - rectWidth / 2.0
        rectPos          = Pos2 rectX (itemY + wpnInfoBackdropOffsetY)
    drawRect rectPos rectWidth wpnInfoBackdropHeight itemPickupOverlayBackdropColor uiInfoTextZIndex

    let
        rectCenterX = rectX + rectWidth / 2.0
        replacePos  = Pos2 rectCenterX (itemY + replaceTextOffsetY)
        wpnLineX    = rectCenterX - wpnLinesMaxWidth / 2.0
        text1Pos    = Pos2 wpnLineX (itemY + wpnLine1TextOffsetY)
        text2Pos    = Pos2 wpnLineX (itemY + wpnLine2TextOffsetY)

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
isBuyConfirmOnInteract = isPlayerEquipmentInfoWeaponsFull <$> readPlayerEquipmentInfo

mkWeaponItemPickupData
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => WeaponType
    -> PlayerMsgPayload
    -> GoldValue
    -> FilePath
    -> RoomType
    -> m ItemPickupData
mkWeaponItemPickupData typ buyMsgPayload cost imgFileName roomType =
    mkItemPickupData typ buyMsgPayload cost imgFileName roomType <&> \itemData -> itemData
        { _isBuyConfirmOnInteract  = isBuyConfirmOnInteract
        , _buyConfirmStartMessages = buyConfirmStartMessages
        , _thinkBuyConfirm         = thinkBuyConfirm
        , _updateBuyConfirm        = updateBuyConfirm
        , _drawBuyConfirmOverlay   = drawBuyConfirmOverlay
        }

mkWeaponItemPickup :: MonadIO m => Pos2 -> ItemPickupData -> m (Some RoomItem)
mkWeaponItemPickup pos itemData = mkItemPickup pos WeaponPickupItemType itemData

mkSwordItemPickup :: RoomType -> Pos2 -> AppEnv p (Some RoomItem)
mkSwordItemPickup roomType pos = do
    swordCost      <- readConfig _level _itemPickupWeaponGoldValue
    buyMsgPayload  <- PlayerMsgBuyWeapon <$> mkSwordWeapon <*> pure swordCost
    itemPickupData <- mkWeaponItemPickupData SwordWeapon buyMsgPayload swordCost swordPickupImgFileName roomType
    mkWeaponItemPickup pos itemPickupData
