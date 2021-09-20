module Level.Room.Item.Pickup
    ( module Level.Room.Item.Pickup.Types
    , mkItemPickup
    , mkItemPickupData
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execStateT, get, lift, modify, put)
import Data.Maybe             (fromMaybe)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T

import Collision.Hitbox
import Constants
import FileCache
import Id
import InfoMsg.Util
import Level.Room.Item as RI
import Level.Room.Item.Pickup.Types
import Level.Room.Types
import Level.Room.Util
import Msg
import Player.EquipmentInfo
import Player.Upgrade
import Player.Upgrade.Manager
import Util
import Window.Graphics
import Window.InputState
import World.Util
import World.ZIndex

itemPickupGravity             = 2460.0 :: Float
emptyReplaceText              = " "    :: T.Text
startingShopAdditionalOffsetY = 40.0   :: OffsetY
indicatorSpriteOffsetX        = 80.0   :: OffsetX

buyPromptOverlayBackdropColor      = Color 0 0 0 200                        :: Color
buyPromptOverlayBackdropBorderSize = 20.0                                   :: Float
buyPromptOverlayBackdropOffsetY    = -245.0                                 :: OffsetY
buyPromptOverlayBackdropHeight     = 100.0                                  :: Float
buyPromptOverlayText0OffsetY       = buyPromptOverlayBackdropOffsetY + 28.0 :: OffsetY
buyPromptOverlayText1OffsetY       = buyPromptOverlayText0OffsetY + 44.0    :: OffsetY

buyPromptReplaceOverlayBackdropHeight = 124.0                                      :: Float
buyPromptReplaceOverlayText0OffsetY   = buyPromptOverlayBackdropOffsetY + 18.0     :: OffsetY
buyPromptReplaceOverlayText1OffsetY   = buyPromptReplaceOverlayText0OffsetY + 34.0 :: OffsetY
buyPromptReplaceOverlayText2OffsetY   = buyPromptReplaceOverlayText1OffsetY + 44.0 :: OffsetY
buyPromptReplaceEquipmentTextColor    = Color 192 192 192 255                      :: Color

costOverlayBackdropOffsetY = -127.0                            :: OffsetY
costOverlayBackdropHeight  = 58.0                              :: Float
costOverlayTextOffsetY     = costOverlayBackdropOffsetY + 29.0 :: OffsetY

buySoundPath     = "event:/SFX Events/Level/pickup-item-buy"      :: FilePath
cantBuySoundPath = "event:/SFX Events/Level/pickup-item-cant-buy" :: FilePath

packPath            = \f -> PackResourceFilePath "data/levels/level-items.pack" f
indicatorSpritePath = packPath "item-pickup-indicator.spr" :: PackResourceFilePath
reappearSpritePath  = packPath "item-pickup-appear.spr"    :: PackResourceFilePath

mkBuyPromptItemNameInputDisplayText
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => T.Text
    -> m InputDisplayText
mkBuyPromptItemNameInputDisplayText name =
    let txt = "Buy " <> name <> ": {InteractAlias}"
    in mkInputDisplayText txt Font32 whiteColor

mkBuyPromptItemCostInputDisplayText
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => GoldValue
    -> m InputDisplayText
mkBuyPromptItemCostInputDisplayText goldValue =
    let txt = "{GoldSymbol} " <> prettyShow goldValue
    in mkInputDisplayText txt Font32 goldTextColor

mkReplaceDisplayText :: (GraphicsRead m, MonadIO m) => T.Text -> m DisplayText
mkReplaceDisplayText txt = mkDisplayText txt Font22 buyPromptReplaceEquipmentTextColor

mkItemPickupData
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m, PrettyShow a)
    => a
    -> PlayerMsgPayload
    -> GoldValue
    -> FilePath
    -> RoomType
    -> m ItemPickupData
mkItemPickupData typ buyMsgPayload cost imgFileName roomType = do
    let name = prettyShow typ

    img                    <- loadPackImage $ packPath imgFileName
    costInputDisplayTxt    <- mkBuyPromptItemCostInputDisplayText cost
    buyInfoInputDisplayTxt <- mkBuyPromptItemNameInputDisplayText name
    replaceDisplayTxt      <- mkReplaceDisplayText emptyReplaceText

    return $ ItemPickupData
        { _name                    = name
        , _buyMsgPayload           = buyMsgPayload
        , _cost                    = cost
        , _touchingPlayer          = False
        , _roomType                = roomType
        , _image                   = img
        , _costInputDisplayText    = costInputDisplayTxt
        , _buyInfoInputDisplayText = buyInfoInputDisplayTxt
        , _replaceDisplayText      = replaceDisplayTxt
        , _status                  = ItemPickupNormalStatus
        }

mkItemPickup :: MonadIO m => Pos2 -> RoomItemType -> ItemPickupData -> m (Some RoomItem)
mkItemPickup (Pos2 x y) itemType itemData =
    let
        width  = imageWidth $ _image itemData
        height = imageHeight $ _image itemData
        pos    = Pos2 (x - width / 2.0) (y - height)
        hitbox = rectHitbox pos width height
    in do
        msgId <- newId
        return . Some $ (mkRoomItem itemType itemData msgId hitbox)
            { _think           = thinkItemPickup
            , _update          = updateItemPickup
            , _draw            = drawItemPickup
            , _playerCollision = itemPickupPlayerCollision
            }

thinkItemPickup :: MsgsRead ThinkLevelMsgsPhase m => RoomItemThink ItemPickupData m
thinkItemPickup item = case _status itemData of
    ItemPickupReappearStatus _           -> return []
    _
        | not (_touchingPlayer itemData) -> return []
        | otherwise                      ->
            let
                interactPressed :: [PlayerMsgPayload] -> Maybe GoldValue
                interactPressed []     = Nothing
                interactPressed (d:ds) = case d of
                    PlayerMsgInteract gold -> Just gold
                    _                      -> interactPressed ds

                roomType      = _roomType itemData
                itemId        = RI._msgId item
                itemRemoveMsg = mkMsg $ RoomMsgRemoveItem itemId
                itemBuyMsg    = mkMsg $ _buyMsgPayload itemData
                itemCost      = _cost itemData
                itemPos       = hitboxCenter $ RI._hitbox item
            in interactPressed <$> readMsgs >>= \case
                Nothing -> return []

                Just playerGoldValue
                    | playerGoldValue >= itemCost -> do
                        playerEquipment <- readPlayerEquipmentInfo

                        let
                            commonMsgs =
                                [ itemBuyMsg
                                , mkMsg $ AudioMsgPlaySound buySoundPath itemPos
                                ]

                        return . (commonMsgs ++) $ if
                                | roomType == startingShopRoomType -> [itemRemoveMsg]
                                | otherwise                        -> case _buyMsgPayload itemData of
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
                                                , mkMsg $ RoomMsgReappearItem itemId
                                                ]

                                    PlayerMsgBuyHealth _ -> [itemRemoveMsg]

                                    _ -> []

                    | otherwise -> return
                        [ mkMsg $ AudioMsgPlaySound cantBuySoundPath itemPos
                        , mkMsg UiMsgInsufficientGold
                        ]

    where itemData = _data item

updateItemPickup
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsRead UpdateLevelMsgsPhase m)
    => RoomItemUpdate ItemPickupData m
updateItemPickup item = flip execStateT item $ do
    modify updateItemPickupGravity
    get >>= lift . updateItemPickupCollision >>= put
    get >>= lift . updateItemPickupDisplayText >>= put
    get >>= lift . updateItemPickupStatus >>= put

updateItemPickupStatus
    :: forall m. (FileCache m, GraphicsRead m, MonadIO m, MsgsRead UpdateLevelMsgsPhase m)
    => RoomItem ItemPickupData
    -> m (RoomItem ItemPickupData)
updateItemPickupStatus item = case _status itemData of
    ItemPickupNormalStatus ->
        let
            processMsgs :: [RoomMsgPayload] -> m (RoomItem ItemPickupData)
            processMsgs []     = return item
            processMsgs (d:ds) = case d of
                RoomMsgShowPickupItemIndicator -> do
                    indicatorSpr <- loadPackSprite indicatorSpritePath
                    return $ item {_data = itemData {_status = ItemPickupIndicatorStatus indicatorSpr}}

                RoomMsgReappearItem msgId
                    | RI._msgId item == msgId -> do
                        reappearSpr <- loadPackSprite reappearSpritePath
                        return $ item {_data = itemData {_status = ItemPickupReappearStatus reappearSpr}}

                _ -> processMsgs ds

        in processMsgs =<< readMsgs

    ItemPickupIndicatorStatus indicatorSpr -> return $ item
        { _data = itemData {_status = ItemPickupIndicatorStatus $ updateSprite indicatorSpr}
        }

    ItemPickupReappearStatus reappearSpr ->
        let
            status
                | spriteFinished reappearSpr = ItemPickupNormalStatus
                | otherwise                  = ItemPickupReappearStatus $ updateSprite reappearSpr
        in return $ item {_data = itemData {_status = status}}

    where itemData = _data item

readPlayerEquipmentInfo :: (AllowMsgRead p InfoMsgPayload, MsgsRead p m) => m PlayerEquipmentInfo
readPlayerEquipmentInfo = processMsg <$> readMsgs
    where
        processMsg :: [InfoMsgPayload] -> PlayerEquipmentInfo
        processMsg []     = mkEmptyPlayerEquipmentInfo
        processMsg (d:ds) = case d of
            InfoMsgPlayer playerInfo -> _equipment playerInfo
            _                        -> processMsg ds

readReplaceText :: MsgsRead UpdateLevelMsgsPhase m => ItemPickupData -> m (Maybe T.Text)
readReplaceText itemData = do
    playerEquipment <- readPlayerEquipmentInfo
    let
        weaponTypes        = _weaponTypes playerEquipment
        gunTypes           = _gunTypes playerEquipment
        movementSkillTypes = _movementSkillTypes playerEquipment

        formatText :: T.Text -> T.Text
        formatText name = "(Replace " <> name <> ")"

    return $ case _buyMsgPayload itemData of
        PlayerMsgBuyWeapon _ _
            | (wpnType:_) <- weaponTypes, length weaponTypes >= maxEquipWeapons ->
                Just $ formatText (prettyShow wpnType)

        PlayerMsgBuyGun _ _
            | (gunType:_) <- gunTypes, length gunTypes >= maxEquipGuns -> Just $ formatText (prettyShow gunType)

        PlayerMsgBuyMovementSkill _ _
            | (moveSkillType:_) <- movementSkillTypes -> Just $ formatText (prettyShow moveSkillType)

        _ -> Nothing

updateItemPickupDisplayText
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsRead UpdateLevelMsgsPhase m)
    => RoomItem ItemPickupData
    -> m (RoomItem ItemPickupData)
updateItemPickupDisplayText item = do
    let itemData            = _data item
    buyInfoInputDisplayTxt <- updateInputDisplayText $ _buyInfoInputDisplayText itemData
    replaceTxt             <- fromMaybe emptyReplaceText <$> readReplaceText itemData

    return $ item
        { _data = itemData
            { _buyInfoInputDisplayText = buyInfoInputDisplayTxt
            , _replaceDisplayText      = updateDisplayText replaceTxt (_replaceDisplayText itemData)
            }
        }

updateItemPickupCollision :: MsgsRead UpdateLevelMsgsPhase m => RoomItem ItemPickupData -> m (RoomItem ItemPickupData)
updateItemPickupCollision item =
    let
        processCollisionMsg :: RoomItem ItemPickupData -> CollisionMsgPayload -> RoomItem ItemPickupData
        processCollisionMsg i d = case d of
            CollisionMsgTouchingGround groundY _
                | velY >= 0.0 ->
                    let
                        hitbox = RI._hitbox i
                        x      = vecX $ hitboxTopLeft hitbox
                        y      = groundY - hitboxHeight hitbox
                    in i
                        { _hitbox = setHitboxTopLeft (Pos2 x y) hitbox
                        , _vel    = Vel2 velX 0.0
                        }
                | otherwise   -> i
            _                 -> i
            where (Vel2 velX velY) = _vel i
    in flip execStateT item $ do
        modify $ \i -> i
            { _data = (_data i) {_touchingPlayer = False}
            }
        get >>= \i ->
            L.foldl' processCollisionMsg i <$> lift (readMsgsTo $ RI._msgId i) >>=
            put

updateItemPickupGravity :: RoomItem ItemPickupData -> RoomItem ItemPickupData
updateItemPickupGravity item = item
    { _hitbox = setHitboxTopLeft (Pos2 x y') hitbox
    , _vel    = Vel2 velX velY'
    }
    where
        hitbox         = RI._hitbox item
        Pos2 x y       = hitboxTopLeft hitbox
        Vel2 velX velY = _vel item
        velY'          = velY + itemPickupGravity * timeStep
        y'             = y + velY' * timeStep

itemPickupPlayerCollision :: RoomItemPlayerCollision ItemPickupData
itemPickupPlayerCollision _ item = [mkMsgTo (RoomMsgUpdateItem updateTouching) (RI._msgId item)]
    where
        updateTouching = \i -> i
            { _data = (_data i) {_touchingPlayer = True}
            }

drawItemPickup :: (GraphicsReadWrite m, InputRead m, MonadIO m) => RoomItemDraw ItemPickupData m
drawItemPickup item =
    let
        hbx      = RI._hitbox item
        imgPos   = hitboxTopLeft hbx
        itemData = _data item
        img      = _image itemData

        drawNormal = do
            drawImage imgPos RightDir levelItemZIndex img
            if
                | _touchingPlayer itemData -> drawBuyPromptOverlay item
                | otherwise                -> drawCostOverlay item
    in case _status itemData of
        ItemPickupNormalStatus -> drawNormal

        ItemPickupIndicatorStatus indicatorSpr ->
            let
                leftPos  = hitboxCenter hbx `vecAdd` Pos2 (-indicatorSpriteOffsetX) 0.0
                rightPos = hitboxCenter hbx `vecAdd` Pos2 indicatorSpriteOffsetX 0.0
            in do
                drawNormal
                drawSprite leftPos RightDir levelItemZIndex indicatorSpr
                drawSprite rightPos LeftDir levelItemZIndex indicatorSpr

        ItemPickupReappearStatus indicatorSpr -> drawSprite imgPos RightDir levelItemZIndex indicatorSpr

drawBuyPromptOverlay :: (GraphicsReadWrite m, InputRead m, MonadIO m) => RoomItem ItemPickupData -> m ()
drawBuyPromptOverlay item = do
    let
        itemData               = _data item
        costInputDisplayTxt    = _costInputDisplayText itemData
        buyInfoInputDisplayTxt = _buyInfoInputDisplayText itemData
        replaceDisplayTxt      = _replaceDisplayText itemData
        isReplaceTextEmpty     = _text (replaceDisplayTxt :: DisplayText) == emptyReplaceText

    widths <- sequenceA
        [ displayTextWidth replaceDisplayTxt
        , inputDisplayTextWidth buyInfoInputDisplayTxt
        , inputDisplayTextWidth costInputDisplayTxt
        ]

    let
        roomTypeOffsetY
            | _roomType itemData == startingShopRoomType = startingShopAdditionalOffsetY
            | otherwise                                  = 0.0

        Pos2 x y                 = hitboxCenter $ RI._hitbox item
        (y', rectHeight)
            | isReplaceTextEmpty = (y + roomTypeOffsetY, buyPromptOverlayBackdropHeight)
            | otherwise          =
                ( y - (buyPromptReplaceOverlayBackdropHeight - buyPromptOverlayBackdropHeight) + roomTypeOffsetY
                , buyPromptReplaceOverlayBackdropHeight
                )

        rectWidth = fromMaybe 0.0 (maybeMaximum widths) + buyPromptOverlayBackdropBorderSize * 2.0
        rectX     = x - rectWidth / 2.0
        rectPos   = Pos2 rectX (y' + buyPromptOverlayBackdropOffsetY)
    drawRect rectPos rectWidth rectHeight buyPromptOverlayBackdropColor uiInfoTextZIndex

    if
        | isReplaceTextEmpty ->
            let
                textPos0 = Pos2 x (y' + buyPromptOverlayText0OffsetY)
                textPos1 = Pos2 x (y' + buyPromptOverlayText1OffsetY)
            in do
                drawInputDisplayTextCentered textPos0 uiInfoTextZIndex buyInfoInputDisplayTxt
                drawInputDisplayTextCentered textPos1 uiInfoTextZIndex costInputDisplayTxt

        | otherwise ->
            let
                textPos0 = Pos2 x (y' + buyPromptReplaceOverlayText0OffsetY)
                textPos1 = Pos2 x (y' + buyPromptReplaceOverlayText1OffsetY)
                textPos2 = Pos2 x (y' + buyPromptReplaceOverlayText2OffsetY)
            in do
                drawDisplayTextCentered textPos0 uiInfoTextZIndex replaceDisplayTxt
                drawInputDisplayTextCentered textPos1 uiInfoTextZIndex buyInfoInputDisplayTxt
                drawInputDisplayTextCentered textPos2 uiInfoTextZIndex costInputDisplayTxt

drawCostOverlay :: (GraphicsReadWrite m, InputRead m, MonadIO m) => RoomItem ItemPickupData -> m ()
drawCostOverlay item = do
    let costInputDisplayTxt   = _costInputDisplayText $ _data item
    costInputDisplayTxtWidth <- inputDisplayTextWidth costInputDisplayTxt

    let
        Pos2 x y  = hitboxCenter $ RI._hitbox item
        rectWidth = costInputDisplayTxtWidth + buyPromptOverlayBackdropBorderSize * 2.0
        rectX     = x - rectWidth / 2.0
        rectPos   = Pos2 rectX (y + costOverlayBackdropOffsetY)
    drawRect rectPos rectWidth costOverlayBackdropHeight buyPromptOverlayBackdropColor uiInfoTextZIndex

    let textPos = Pos2 x (y + costOverlayTextOffsetY)
    drawInputDisplayTextCentered textPos uiInfoTextZIndex costInputDisplayTxt
