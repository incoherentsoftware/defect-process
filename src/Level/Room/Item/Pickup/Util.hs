module Level.Room.Item.Pickup.Util
    ( itemPickupOverlayBackdropColor
    , itemPickupOverlayBackdropBorderSize
    , itemPickupRemoveMessage
    , itemPickupBuyMessages
    , itemPickupCantBuySoundMessage
    , itemPickupBuyConfirmSoundMessage
    , drawItemPickupBuyConfirmControlsOverlay
    , drawItemPickupBuyConfirmMinimalControlsOverlay
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

import Collision.Hitbox
import Level.Room.Item as RI
import Level.Room.Item.Pickup.Types
import Level.Room.Util
import Msg
import Player.EquipmentInfo
import Player.Upgrade
import Player.Upgrade.Manager
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

itemPickupOverlayBackdropColor       = Color 0 0 0 200 :: Color
itemPickupOverlayBackdropBorderSize  = 20.0            :: Float

controlsOverlayBackdropHeight        = 195.0  :: Float
controlsOverlayBackdropOffsetY       = -340.0 :: OffsetY
controlsOverlaySpacerWidth           = 30.0   :: Float
selectDisplayTextOffsetY             = -313.0 :: OffsetY
upAliasInputDisplayTextOffsetY       = -319.0 :: OffsetY
downAliasInputDisplayTextOffsetY     = -263.0 :: OffsetY
confirmInputDisplayTextOffsetY       = -190.0 :: OffsetY
interactAliasInputDisplayTextOffsetY = -196.0 :: OffsetY

minimalControlsOverlayBackdropHeight  = 68.0   :: Float
minimalControlsOverlayBackdropOffsetY = -213.0 :: OffsetY

cantBuySoundPath    = "event:/SFX Events/Level/pickup-item-cant-buy" :: FilePath
buySoundPath        = "event:/SFX Events/Level/pickup-item-buy"      :: FilePath
buyConfirmSoundPath = "event:/SFX Events/UI/skill-slot-select"       :: FilePath

itemPickupRemoveMessage :: RoomItem ItemPickupData -> Msg ThinkLevelMsgsPhase
itemPickupRemoveMessage item = mkMsg $ RoomMsgRemoveItem (RI._msgId item)

itemPickupBuyMessages :: MsgsRead ThinkLevelMsgsPhase m => RoomItem ItemPickupData -> m [Msg ThinkLevelMsgsPhase]
itemPickupBuyMessages item = buyMessages <$> readPlayerEquipmentInfo
    where
        roomType      = _roomType $ RI._data item
        itemPos       = hitboxCenter $ RI._hitbox item
        itemRemoveMsg = itemPickupRemoveMessage item
        buyMsgPayload = _buyMsgPayload $ RI._data item
        commonMsgs    =
            [ mkMsg buyMsgPayload
            , mkMsg $ AudioMsgPlaySound buySoundPath itemPos
            ]

        buyMessages :: PlayerEquipmentInfo -> [Msg ThinkLevelMsgsPhase]
        buyMessages playerEquipment = (commonMsgs ++) $ if
            | roomType == startingShopRoomType -> [itemRemoveMsg]
            | roomType == tutorialRoomType     -> [itemRemoveMsg]
            | otherwise                        -> case buyMsgPayload of
                PlayerMsgBuyWeapon _ _ ->
                    let weaponCount = length (_weaponTypes playerEquipment) + 1
                    in
                        [ mkMsg $ UiMsgShowWeaponEquipmentInfo weaponCount
                        , itemRemoveMsg
                        ]

                PlayerMsgBuyGun _ _ ->
                    let gunCount = length (_gunTypes playerEquipment) + 1
                    in
                        [ mkMsg $ UiMsgShowGunEquipmentInfo gunCount
                        , itemRemoveMsg
                        ]

                PlayerMsgBuyMovementSkill _ _ ->
                    [ mkMsg UiMsgShowGeneralEquipmentInfo
                    , itemRemoveMsg
                    ]

                PlayerMsgBuySecondarySkill _ _ ->
                    [ mkMsg $ UiMsgShowSecondarySkillEquipmentInfo playerEquipment
                    , itemRemoveMsg
                    ]

                PlayerMsgBuyUpgrade upgradeType _ ->
                    let
                        isMeterUpgrade    = upgradeType == MeterUpgradeType
                        upgradeCounts     = _upgradeCounts playerEquipment
                        meterUpgradeCount = M.findWithDefault 0 MeterUpgradeType upgradeCounts
                    in if
                        | isMeterUpgrade && meterUpgradeCount + 1 >= maxMeterUpgradeCount ->
                            [ mkMsg UiMsgShowGeneralEquipmentInfo
                            , itemRemoveMsg
                            ]
                        | otherwise                                                       ->
                            [ mkMsg UiMsgShowGeneralEquipmentInfo
                            , mkMsg $ RoomMsgReappearItem (RI._msgId item)
                            ]

                PlayerMsgBuyHealth _ -> [itemRemoveMsg]

                _ -> []

itemPickupCantBuySoundMessage :: RoomItem ItemPickupData -> Msg ThinkLevelMsgsPhase
itemPickupCantBuySoundMessage item = mkMsg $ AudioMsgPlaySound cantBuySoundPath itemPos
    where itemPos = hitboxCenter $ RI._hitbox item

itemPickupBuyConfirmSoundMessage :: RoomItem ItemPickupData -> Msg ThinkLevelMsgsPhase
itemPickupBuyConfirmSoundMessage _ = mkMsg $ AudioMsgPlaySoundCentered buyConfirmSoundPath

drawItemPickupBuyConfirmControlsOverlay
    :: (GraphicsReadWrite m, InputRead m, MonadIO m)
    => PosX
    -> ItemPickupDrawBuyConfirm m
drawItemPickupBuyConfirmControlsOverlay x item = do
    let
        buyConfirmData               = _buyConfirmData $ RI._data item
        selectDisplayTxt             = _selectDisplayText buyConfirmData
        upAliasInputDisplayTxt       = _upAliasInputDisplayText buyConfirmData
        downAliasInputDisplayTxt     = _downAliasInputDisplayText buyConfirmData
        confirmDisplayTxt            = _confirmDisplayText buyConfirmData
        interactAliasInputDisplayTxt = _interactAliasInputDisplayText buyConfirmData
    selectDisplayTxtWidth             <- displayTextWidth selectDisplayTxt
    upAliasInputDisplayTxtWidth       <- inputDisplayTextWidth upAliasInputDisplayTxt
    downAliasInputDisplayTxtWidth     <- inputDisplayTextWidth downAliasInputDisplayTxt
    confirmDisplayTxtWidth            <- displayTextWidth confirmDisplayTxt
    interactAliasInputDisplayTxtWidth <- inputDisplayTextWidth interactAliasInputDisplayTxt

    let
        leftMaxWidth  = max selectDisplayTxtWidth confirmDisplayTxtWidth
        rightMaxWidth = maximum $ NE.fromList
            [ upAliasInputDisplayTxtWidth
            , downAliasInputDisplayTxtWidth
            , interactAliasInputDisplayTxtWidth
            ]
        rectX         = x + controlsOverlaySpacerWidth
        rectWidth     = leftMaxWidth + rightMaxWidth + itemPickupOverlayBackdropBorderSize * 2.0
        itemY         = vecY $ hitboxCenter (RI._hitbox item)
        rectPos       = Pos2 rectX (itemY + controlsOverlayBackdropOffsetY)
    drawRect rectPos rectWidth controlsOverlayBackdropHeight itemPickupOverlayBackdropColor uiInfoTextZIndex

    let
        colonX               = rectX + itemPickupOverlayBackdropBorderSize + leftMaxWidth
        selectTextPos        = Pos2 colonX (itemY + selectDisplayTextOffsetY)
        upAliasTextPos       = Pos2 colonX (itemY + upAliasInputDisplayTextOffsetY)
        downAliasTextPos     = Pos2 colonX (itemY + downAliasInputDisplayTextOffsetY)
        confirmTextPos       = Pos2 colonX (itemY + confirmInputDisplayTextOffsetY)
        interactAliasTextPos = Pos2 colonX (itemY + interactAliasInputDisplayTextOffsetY)
    drawDisplayTextRightAligned selectTextPos uiInfoTextZIndex selectDisplayTxt
    drawInputDisplayText upAliasTextPos uiInfoTextZIndex upAliasInputDisplayTxt
    drawInputDisplayText downAliasTextPos uiInfoTextZIndex downAliasInputDisplayTxt
    drawDisplayTextRightAligned confirmTextPos uiInfoTextZIndex confirmDisplayTxt
    drawInputDisplayText interactAliasTextPos uiInfoTextZIndex interactAliasInputDisplayTxt

drawItemPickupBuyConfirmMinimalControlsOverlay
    :: (GraphicsReadWrite m, InputRead m, MonadIO m)
    => PosX
    -> ItemPickupDrawBuyConfirm m
drawItemPickupBuyConfirmMinimalControlsOverlay x item = do
    let
        buyConfirmData               = _buyConfirmData $ RI._data item
        confirmDisplayTxt            = _confirmDisplayText buyConfirmData
        interactAliasInputDisplayTxt = _interactAliasInputDisplayText buyConfirmData
    confirmDisplayTxtWidth            <- displayTextWidth confirmDisplayTxt
    interactAliasInputDisplayTxtWidth <- inputDisplayTextWidth interactAliasInputDisplayTxt

    let
        rectX     = x + controlsOverlaySpacerWidth
        rectWidth =
            confirmDisplayTxtWidth + interactAliasInputDisplayTxtWidth + itemPickupOverlayBackdropBorderSize * 2.0
        itemY     = vecY $ hitboxCenter (RI._hitbox item)
        rectPos   = Pos2 rectX (itemY + minimalControlsOverlayBackdropOffsetY)
    drawRect rectPos rectWidth minimalControlsOverlayBackdropHeight itemPickupOverlayBackdropColor uiInfoTextZIndex

    let
        colonX               = rectX + itemPickupOverlayBackdropBorderSize + confirmDisplayTxtWidth
        confirmTextPos       = Pos2 colonX (itemY + confirmInputDisplayTextOffsetY)
        interactAliasTextPos = Pos2 colonX (itemY + interactAliasInputDisplayTextOffsetY)
    drawDisplayTextRightAligned confirmTextPos uiInfoTextZIndex confirmDisplayTxt
    drawInputDisplayText interactAliasTextPos uiInfoTextZIndex interactAliasInputDisplayTxt
