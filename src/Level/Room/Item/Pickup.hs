module Level.Room.Item.Pickup
    ( module Level.Room.Item.Pickup.Types
    , mkItemPickup
    , mkItemPickupData
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execStateT, get, lift, modify, put)
import Data.Maybe             (fromMaybe)
import qualified Data.List as L
import qualified Data.Text as T

import AppEnv
import Collision.Hitbox
import Constants
import FileCache
import Id
import Level.Room.Item as RI
import Level.Room.Item.Pickup.Types
import Level.Room.Item.Pickup.Util
import Level.Room.Types
import Level.Room.Util
import Msg
import Util
import Window.Graphics
import Window.InputState
import World.Util
import World.ZIndex

itemPickupGravity             = 2460.0 :: Float
startingShopAdditionalOffsetY = 40.0   :: OffsetY
indicatorSpriteOffsetX        = 80.0   :: OffsetX

buyPromptOverlayBackdropColor      = Color 0 0 0 200                        :: Color
buyPromptOverlayBackdropBorderSize = 20.0                                   :: Float
buyPromptOverlayBackdropOffsetY    = -245.0                                 :: OffsetY
buyPromptOverlayBackdropHeight     = 100.0                                  :: Float
buyPromptOverlayText0OffsetY       = buyPromptOverlayBackdropOffsetY + 28.0 :: OffsetY
buyPromptOverlayText1OffsetY       = buyPromptOverlayText0OffsetY + 44.0    :: OffsetY

costOverlayBackdropOffsetY = -127.0                            :: OffsetY
costOverlayBackdropHeight  = 58.0                              :: Float
costOverlayTextOffsetY     = costOverlayBackdropOffsetY + 29.0 :: OffsetY

literalEmptyText      = "empty"               :: T.Text
selectText            = "Select:"             :: T.Text
confirmText           = "Confirm:"            :: T.Text
literalEmptyTextColor = Color 100 100 100 255 :: Color
textSelectedColor     = Color 255 235 79 255  :: Color
replaceTextColor      = Color 192 192 192 255 :: Color

uiPackPath                           = \f -> PackResourceFilePath "data/ui/ui.pack" f
slotsOverlayNeutralSelectedImagePath =
    uiPackPath "secondary-skill-view-info-overlay-neutral-selected.image" :: PackResourceFilePath
slotsOverlayUpSelectedImagePath      =
    uiPackPath "secondary-skill-view-info-overlay-up-selected.image"      :: PackResourceFilePath
slotsOverlayDownSelectedImagePath    =
    uiPackPath "secondary-skill-view-info-overlay-down-selected.image"    :: PackResourceFilePath

lvlPackPath         = \f -> PackResourceFilePath "data/levels/level-items.pack" f
indicatorSpritePath = lvlPackPath "item-pickup-indicator.spr" :: PackResourceFilePath
reappearSpritePath  = lvlPackPath "item-pickup-appear.spr"    :: PackResourceFilePath

mkBuyPromptItemNameInputDisplayText
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => T.Text
    -> m InputDisplayText
mkBuyPromptItemNameInputDisplayText name = mkInputDisplayText txt Font32 whiteColor
    where txt = "Buy " <> name <> ": {InteractAlias}"

mkBuyPromptItemCostInputDisplayText
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => GoldValue
    -> m InputDisplayText
mkBuyPromptItemCostInputDisplayText goldValue = mkInputDisplayText txt Font32 goldTextColor
    where txt = "{GoldSymbol} " <> prettyShow goldValue

mkItemPickupBuyConfirmData :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m) => m ItemPickupBuyConfirmData
mkItemPickupBuyConfirmData = do
    selectDisplayTxt             <- mkDisplayText selectText Font29 whiteColor
    upAliasInputDisplayTxt       <- mkInputDisplayText " {UpAlias}" Font32 whiteColor
    downAliasInputDisplayTxt     <- mkInputDisplayText " {DownAlias}" Font32 whiteColor
    confirmDisplayTxt            <- mkDisplayText confirmText Font29 whiteColor
    interactAliasInputDisplayTxt <- mkInputDisplayText " {InteractAlias}" Font32 whiteColor

    replace0DisplayTxt      <- mkDisplayText "" Font26 replaceTextColor
    replace1DisplayTxt      <- mkDisplayText "" Font26 replaceTextColor
    replace2DisplayTxt      <- mkDisplayText "" Font26 replaceTextColor
    line0DisplayTxt         <- mkDisplayText "" Font29 whiteColor
    line1DisplayTxt         <- mkDisplayText "" Font29 whiteColor
    line1SelectedDisplayTxt <- mkDisplayText "" Font29 textSelectedColor
    line2DisplayTxt         <- mkDisplayText "" Font29 whiteColor
    line2SelectedDisplayTxt <- mkDisplayText "" Font29 textSelectedColor
    line3DisplayTxt         <- mkDisplayText "" Font29 whiteColor
    line3SelectedDisplayTxt <- mkDisplayText "" Font29 textSelectedColor
    line4DisplayTxt         <- mkDisplayText "" Font29 whiteColor

    slotsOverlayNeutralSelectedImg <- loadPackImage slotsOverlayNeutralSelectedImagePath
    slotsOverlayUpSelectedImg      <- loadPackImage slotsOverlayUpSelectedImagePath
    slotsOverlayDownSelectedImg    <- loadPackImage slotsOverlayDownSelectedImagePath
    literalEmptyDisplayTxt         <- mkDisplayText literalEmptyText Font29 literalEmptyTextColor

    return $ ItemPickupBuyConfirmData
        { _literalEmptyText                 = literalEmptyDisplayTxt
        , _selectDisplayText                = selectDisplayTxt
        , _upAliasInputDisplayText          = upAliasInputDisplayTxt
        , _downAliasInputDisplayText        = downAliasInputDisplayTxt
        , _confirmDisplayText               = confirmDisplayTxt
        , _interactAliasInputDisplayText    = interactAliasInputDisplayTxt
        , _selectedLineIndex                = 0
        , _replace0DisplayText              = replace0DisplayTxt
        , _replace1DisplayText              = replace1DisplayTxt
        , _replace2DisplayText              = replace2DisplayTxt
        , _line0DisplayText                 = line0DisplayTxt
        , _line1DisplayText                 = line1DisplayTxt
        , _line1SelectedDisplayText         = line1SelectedDisplayTxt
        , _line2DisplayText                 = line2DisplayTxt
        , _line2SelectedDisplayText         = line2SelectedDisplayTxt
        , _line3DisplayText                 = line3DisplayTxt
        , _line3SelectedDisplayText         = line3SelectedDisplayTxt
        , _line4DisplayText                 = line4DisplayTxt
        , _slotsOverlayNeutralSelectedImage = slotsOverlayNeutralSelectedImg
        , _slotsOverlayUpSelectedImage      = slotsOverlayUpSelectedImg
        , _slotsOverlayDownSelectedImage    = slotsOverlayDownSelectedImg
        }

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

    img                    <- loadPackImage $ lvlPackPath imgFileName
    costInputDisplayTxt    <- mkBuyPromptItemCostInputDisplayText cost
    buyInfoInputDisplayTxt <- mkBuyPromptItemNameInputDisplayText name
    buyConfirmData         <- mkItemPickupBuyConfirmData

    return $ ItemPickupData
        { _name                    = name
        , _buyMsgPayload           = buyMsgPayload
        , _cost                    = cost
        , _touchingPlayer          = False
        , _roomType                = roomType
        , _image                   = img
        , _costInputDisplayText    = costInputDisplayTxt
        , _buyInfoInputDisplayText = buyInfoInputDisplayTxt
        , _status                  = ItemPickupNormalStatus
        , _buyConfirmData          = buyConfirmData
        , _isBuyConfirmOnInteract  = return False
        , _buyConfirmStartMessages = const $ return []
        , _thinkBuyConfirm         = const $ return []
        , _updateBuyConfirm        = return . _buyConfirmData
        , _drawBuyConfirmOverlay   = const $ return ()
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
            , _inInteractRange = _touchingPlayer . _data
            }

itemPickupSetNormalStatusMessage :: RoomItem ItemPickupData -> Msg ThinkLevelMsgsPhase
itemPickupSetNormalStatusMessage item = mkMsgTo (RoomMsgUpdateItem update) (RI._msgId item)
    where update = \i -> i {RI._data = (RI._data i) {_status = ItemPickupNormalStatus}}

thinkItemPickup :: RoomItemThink ItemPickupData (AppEnv ThinkLevelMsgsPhase)
thinkItemPickup item = case _status itemData of
    ItemPickupReappearStatus _ -> return []

    ItemPickupBuyConfirmStatus
        | not touchingPlayer -> return [itemPickupSetNormalStatusMessage item]
        | otherwise          -> (_thinkBuyConfirm itemData) item

    _
        | not touchingPlayer -> return []
        | otherwise          -> (interactPressed <$> readMsgs) >>= \case
            Nothing -> return []

            Just playerGoldValue
                | playerGoldValue < _cost itemData -> return
                    [ itemPickupCantBuySoundMessage item
                    , mkMsg UiMsgInsufficientGold
                    ]

                | otherwise -> _isBuyConfirmOnInteract itemData >>= \case
                    True -> do
                        buyConfirmStartMsgs <- (_buyConfirmStartMessages itemData) item
                        return $ buyConfirmStartMsgs ++
                            [ itemPickupBuyConfirmSoundMessage item
                            , mkMsg UiMsgHideEquipmentInfo
                            ]

                    False -> do
                        itemBuyMsgs <- itemPickupBuyMessages item
                        let
                            itemRoomSpecificMsgs
                                | _roomType itemData == startingShopRoomType = [itemPickupRemoveMessage item]
                                | otherwise                                  = []
                        return $ itemBuyMsgs ++ itemRoomSpecificMsgs
    where
        interactPressed :: [PlayerMsgPayload] -> Maybe GoldValue
        interactPressed []     = Nothing
        interactPressed (d:ds) = case d of
            PlayerMsgInteract gold -> Just gold
            _                      -> interactPressed ds

        itemData       = RI._data item
        touchingPlayer = _touchingPlayer itemData

updateItemPickup :: RoomItemUpdate ItemPickupData (AppEnv UpdateLevelMsgsPhase)
updateItemPickup item = flip execStateT item $ do
    modify updateItemPickupGravity
    get >>= lift . updateItemPickupCollision >>= put
    get >>= lift . updateItemPickupDisplayText >>= put
    get >>= lift . updateItemPickupStatus >>= put

    get >>= \i -> do
        let iData       = RI._data i
        buyConfirmData <- lift $ (_updateBuyConfirm iData) iData
        put $ i {RI._data = (RI._data i) {_buyConfirmData = buyConfirmData}}

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
                    return $ item {RI._data = itemData {_status = ItemPickupIndicatorStatus indicatorSpr}}

                RoomMsgReappearItem msgId
                    | RI._msgId item == msgId -> do
                        reappearSpr <- loadPackSprite reappearSpritePath
                        return $ item {RI._data = itemData {_status = ItemPickupReappearStatus reappearSpr}}

                _ -> processMsgs ds

        in processMsgs =<< readMsgs

    ItemPickupIndicatorStatus indicatorSpr -> return $ item
        { RI._data = itemData {_status = ItemPickupIndicatorStatus $ updateSprite indicatorSpr}
        }

    ItemPickupReappearStatus reappearSpr ->
        let
            status
                | spriteFinished reappearSpr = ItemPickupNormalStatus
                | otherwise                  = ItemPickupReappearStatus $ updateSprite reappearSpr
        in return $ item {RI._data = itemData {_status = status}}

    ItemPickupBuyConfirmStatus -> return item

    where itemData = RI._data item

updateItemPickupDisplayText
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => RoomItem ItemPickupData
    -> m (RoomItem ItemPickupData)
updateItemPickupDisplayText item = do
    buyInfoInputDisplayTxt <- updateInputDisplayText $ _buyInfoInputDisplayText (RI._data item)
    return $ item
        { RI._data = (RI._data item) {_buyInfoInputDisplayText = buyInfoInputDisplayTxt}
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
            { RI._data = (RI._data i) {_touchingPlayer = False}
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
            { RI._data = (RI._data i) {_touchingPlayer = True}
            }

drawItemPickup :: RoomItemDraw ItemPickupData (AppEnv DrawMsgsPhase)
drawItemPickup item =
    let
        hbx      = RI._hitbox item
        imgPos   = hitboxTopLeft hbx
        itemData = RI._data item
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

        ItemPickupBuyConfirmStatus -> do
            drawImage imgPos RightDir levelItemZIndex img
            (_drawBuyConfirmOverlay itemData) item

drawBuyPromptOverlay :: (GraphicsReadWrite m, InputRead m, MonadIO m) => RoomItem ItemPickupData -> m ()
drawBuyPromptOverlay item = do
    let
        itemData               = RI._data item
        costInputDisplayTxt    = _costInputDisplayText itemData
        buyInfoInputDisplayTxt = _buyInfoInputDisplayText itemData

    widths <- sequenceA
        [ inputDisplayTextWidth buyInfoInputDisplayTxt
        , inputDisplayTextWidth costInputDisplayTxt
        ]

    let
        roomTypeOffsetY
            | _roomType itemData == startingShopRoomType = startingShopAdditionalOffsetY
            | otherwise                                  = 0.0

        Pos2 x y         = hitboxCenter $ RI._hitbox item
        (y', rectHeight) = (y + roomTypeOffsetY, buyPromptOverlayBackdropHeight)

        rectWidth = fromMaybe 0.0 (maybeMaximum widths) + buyPromptOverlayBackdropBorderSize * 2.0
        rectX     = x - rectWidth / 2.0
        rectPos   = Pos2 rectX (y' + buyPromptOverlayBackdropOffsetY)
    drawRect rectPos rectWidth rectHeight buyPromptOverlayBackdropColor uiInfoTextZIndex

    let
        textPos0 = Pos2 x (y' + buyPromptOverlayText0OffsetY)
        textPos1 = Pos2 x (y' + buyPromptOverlayText1OffsetY)
    drawInputDisplayTextCentered textPos0 uiInfoTextZIndex buyInfoInputDisplayTxt
    drawInputDisplayTextCentered textPos1 uiInfoTextZIndex costInputDisplayTxt

drawCostOverlay :: (GraphicsReadWrite m, InputRead m, MonadIO m) => RoomItem ItemPickupData -> m ()
drawCostOverlay item = do
    let costInputDisplayTxt   = _costInputDisplayText $ RI._data item
    costInputDisplayTxtWidth <- inputDisplayTextWidth costInputDisplayTxt

    let
        Pos2 x y  = hitboxCenter $ RI._hitbox item
        rectWidth = costInputDisplayTxtWidth + buyPromptOverlayBackdropBorderSize * 2.0
        rectX     = x - rectWidth / 2.0
        rectPos   = Pos2 rectX (y + costOverlayBackdropOffsetY)
    drawRect rectPos rectWidth costOverlayBackdropHeight buyPromptOverlayBackdropColor uiInfoTextZIndex

    let textPos = Pos2 x (y + costOverlayTextOffsetY)
    drawInputDisplayTextCentered textPos uiInfoTextZIndex costInputDisplayTxt
