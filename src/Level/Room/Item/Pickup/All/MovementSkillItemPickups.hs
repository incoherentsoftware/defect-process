module Level.Room.Item.Pickup.All.MovementSkillItemPickups
    ( module Player.MovementSkill.All
    , mkDashItemPickup
    , mkTeleportItemPickup
    , mkGrappleItemPickup
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))
import Data.Maybe             (listToMaybe)

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
import Player.MovementSkill.All
import Player.MovementSkill.Types as MS
import Util
import Window.Graphics
import Window.InputState
import World.Util
import World.ZIndex

dashPickupImgFileName     = "dash-pickup.image"     :: FileName
teleportPickupImgFileName = "teleport-pickup.image" :: FileName
grapplePickupImgFileName  = "grapple-pickup.image"  :: FileName

moveSkillInfoBackdropOffsetY = -248.0 :: OffsetY
replaceTextOffsetY           = -223.0 :: OffsetY
moveSkillLine1TextOffsetY    = -192.0 :: OffsetY
moveSkillInfoBackdropHeight  = 103.0  :: Float

buyConfirmStartMessages :: Monad m => ItemPickupBuyConfirmStartMessages m
buyConfirmStartMessages item = return [mkMsgTo (RoomMsgUpdateItem update) (RI._msgId item)]
    where
        update = \i -> i
            { RI._data = (RI._data i) {_status = ItemPickupBuyConfirmStatus} :: ItemPickupData
            }

thinkBuyConfirm :: (InputRead m, MsgsRead ThinkLevelMsgsPhase m) => ItemPickupThinkBuyConfirm m
thinkBuyConfirm item = readInputState >>= \inputState -> if
    | InteractAlias `aliasPressed` inputState -> itemPickupBuyMessages item
    | otherwise                               -> return []

calculateMovementSkillSlots
    :: (AllowMsgRead p InfoMsgPayload, MsgsRead p m)
    => ItemPickupData
    -> m (Maybe MovementSkillType, Maybe MovementSkillType)
calculateMovementSkillSlots itemData =
    let
        buyMoveSkillType = case _buyMsgPayload itemData of
            PlayerMsgBuyMovementSkill (Some moveSkill) _ -> Just $ MS._type moveSkill
            _                                            -> Nothing
    in do
        moveSkillTypes <- _movementSkillTypes <$> readPlayerEquipmentInfo
        return (buyMoveSkillType, listToMaybe moveSkillTypes)

updateBuyConfirm
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsRead UpdateLevelMsgsPhase m)
    => ItemPickupUpdateBuyConfirm m
updateBuyConfirm itemData = do
    let buyConfirmData            = _buyConfirmData itemData
    interactAliasInputDisplayTxt <- updateInputDisplayText $ _interactAliasInputDisplayText buyConfirmData

    (line1MoveSkillType, replacedMoveSkillType) <- calculateMovementSkillSlots itemData
    let
        formatMoveSkillText = \t -> maybe "" prettyShow t
        formatReplaceText   = \t -> "(Replace " <> formatMoveSkillText t <> ")"
        moveSkillLine1Txt   = "• " <> formatMoveSkillText line1MoveSkillType
        replace0Txt         = formatReplaceText replacedMoveSkillType

    return $ buyConfirmData
        { _interactAliasInputDisplayText = interactAliasInputDisplayTxt
        , _replace0DisplayText           = updateDisplayText replace0Txt (_replace0DisplayText buyConfirmData)
        , _line1SelectedDisplayText      =
            updateDisplayText moveSkillLine1Txt (_line1SelectedDisplayText buyConfirmData)
        }

drawBuyConfirmOverlay :: (GraphicsReadWrite m, InputRead m, MonadIO m) => ItemPickupDrawBuyConfirm m
drawBuyConfirmOverlay item = do
    let buyConfirmData = _buyConfirmData $ RI._data item

    moveSkillLinesMaxWidth <- displayTextWidth $ _line1SelectedDisplayText buyConfirmData
    replaceMaxWidth        <- displayTextWidth $ _replace0DisplayText buyConfirmData

    let
        Pos2 itemX itemY = hitboxCenter $ RI._hitbox item
        allLinesMaxWidth = max moveSkillLinesMaxWidth replaceMaxWidth
        rectWidth        = allLinesMaxWidth + itemPickupOverlayBackdropBorderSize * 2.0
        rectX            = itemX - rectWidth / 2.0
        rectPos          = Pos2 rectX (itemY + moveSkillInfoBackdropOffsetY)
    drawRect rectPos rectWidth moveSkillInfoBackdropHeight itemPickupOverlayBackdropColor uiInfoTextZIndex

    let
        rectCenterX    = rectX + rectWidth / 2.0
        replacePos     = Pos2 rectCenterX (itemY + replaceTextOffsetY)
        moveSkillLineX = rectCenterX - moveSkillLinesMaxWidth / 2.0
        text1Pos       = Pos2 moveSkillLineX (itemY + moveSkillLine1TextOffsetY)

    drawDisplayTextCentered replacePos uiInfoTextZIndex (_replace0DisplayText buyConfirmData)
    drawDisplayText text1Pos uiInfoTextZIndex (_line1SelectedDisplayText buyConfirmData)

    drawItemPickupBuyConfirmMinimalControlsOverlay (rectX + rectWidth) item

isBuyConfirmOnInteract :: MsgsRead ThinkLevelMsgsPhase m => ItemPickupIsBuyConfirmOnInteract m
isBuyConfirmOnInteract = isPlayerEquipmentInfoMovementSkillFull <$> readPlayerEquipmentInfo

mkMovementSkillItemPickupData
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => MovementSkillType
    -> PlayerMsgPayload
    -> GoldValue
    -> FilePath
    -> RoomType
    -> m ItemPickupData
mkMovementSkillItemPickupData typ buyMsgPayload cost imgFileName roomType =
    mkItemPickupData typ buyMsgPayload cost imgFileName roomType <&> \itemData -> itemData
        { _isBuyConfirmOnInteract  = isBuyConfirmOnInteract
        , _buyConfirmStartMessages = buyConfirmStartMessages
        , _thinkBuyConfirm         = thinkBuyConfirm
        , _updateBuyConfirm        = updateBuyConfirm
        , _drawBuyConfirmOverlay   = drawBuyConfirmOverlay
        }

mkMovementSkillItemPickup :: MonadIO m => Pos2 -> ItemPickupData -> m (Some RoomItem)
mkMovementSkillItemPickup pos itemData = mkItemPickup pos MovementSkillPickupItemType itemData

mkDashItemPickup
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => RoomType
    -> Pos2
    -> m (Some RoomItem)
mkDashItemPickup roomType pos = do
    dashCost       <- readConfig _level _itemPickupMovementSkillGoldValue
    buyMsgPayload  <- PlayerMsgBuyMovementSkill <$> mkDashSkill <*> pure dashCost
    itemPickupData <- mkMovementSkillItemPickupData DashSkill buyMsgPayload dashCost dashPickupImgFileName roomType
    mkMovementSkillItemPickup pos itemPickupData

mkTeleportItemPickup
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => RoomType
    -> Pos2
    -> m (Some RoomItem)
mkTeleportItemPickup roomType pos = do
    teleportCost   <- readConfig _level _itemPickupMovementSkillGoldValue
    buyMsgPayload  <- PlayerMsgBuyMovementSkill <$> mkTeleportSkill <*> pure teleportCost
    itemPickupData <-
        mkMovementSkillItemPickupData TeleportSkill buyMsgPayload teleportCost teleportPickupImgFileName roomType
    mkMovementSkillItemPickup pos itemPickupData

mkGrappleItemPickup
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => RoomType
    -> Pos2
    -> m (Some RoomItem)
mkGrappleItemPickup roomType pos = do
    grappleCost    <- readConfig _level _itemPickupMovementSkillGoldValue
    buyMsgPayload  <- PlayerMsgBuyMovementSkill <$> mkGrappleSkill <*> pure grappleCost
    itemPickupData <-
        mkMovementSkillItemPickupData GrappleSkill buyMsgPayload grappleCost grapplePickupImgFileName roomType
    mkMovementSkillItemPickup pos itemPickupData
