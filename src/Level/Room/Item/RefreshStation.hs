module Level.Room.Item.RefreshStation
    ( mkRefreshStation
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))
import qualified Data.Text as T

import Collision.Hitbox
import FileCache
import Id
import Level.Room.Item
import Level.Room.Item.RefreshStation.Types
import Msg
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

refreshStationWidth          = 86.0   :: Float
refreshStationHeight         = 315.0  :: Float
refreshStationOverlayOffsetY = -171.0 :: PosY

overlayText               = "Refill meter {InteractAlias}"                       :: T.Text
overlayBackdropColor      = Color 0 0 0 200                                      :: Color
overlayBackdropBorderSize = 20.0                                                 :: Float
overlayBackdropOffsetY    = -415.0                                               :: PosY
overlayBackdropHeight     = 60.0                                                 :: Float
overlayTextCenterOffsetY  = overlayBackdropOffsetY + overlayBackdropHeight / 2.0 :: PosY

refillSoundPath = "event:/SFX Events/Level/refresh-station-refill" :: FilePath
drainSoundPath  = "event:/SFX Events/Level/refresh-station-drain"  :: FilePath

packPath = \f -> PackResourceFilePath "data/levels/level-items.pack" f

mkRefreshStationSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m RefreshStationSprites
mkRefreshStationSprites =
    RefreshStationSprites <$>
    loadPackSprite (packPath "refresh-station-meter-fill.spr") <*>
    loadPackSprite (packPath "refresh-station-meter-drain.spr")

mkRefreshStationData :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m) => m RefreshStationData
mkRefreshStationData = do
    sprs                   <- mkRefreshStationSprites
    img                    <- loadPackImage (packPath "refresh-station.image")
    overlayInputDisplayTxt <- mkInputDisplayText overlayText Font32 whiteColor

    return $ RefreshStationData
        { _touchingPlayer          = False
        , _image                   = img
        , _overlaySprite           = finishSprite $ _meterFill sprs
        , _overlayInputDisplayText = overlayInputDisplayTxt
        , _sprites                 = sprs
        }

mkRefreshStation :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m) => Pos2 -> m (Some RoomItem)
mkRefreshStation (Pos2 x y) =
    let
        pos = Pos2 (x - refreshStationWidth / 2.0) (y - refreshStationHeight)
        hbx = rectHitbox pos refreshStationWidth refreshStationHeight
    in do
        refreshStationData <- mkRefreshStationData
        msgId              <- newId

        return . Some $ (mkRoomItem RefreshStationItemType refreshStationData msgId hbx)
            { _draw            = drawRefreshStation
            , _think           = thinkRefreshStation
            , _update          = updateRefreshStation
            , _playerCollision = refreshStationPlayerCollision
            }

isRefreshStationFilled :: RoomItem RefreshStationData -> Bool
isRefreshStationFilled refreshStation = overlaySpr == _meterFill sprs && spriteFinished overlaySpr
    where
        refreshStationData = _data refreshStation
        overlaySpr         = _overlaySprite refreshStationData
        sprs               = _sprites refreshStationData

thinkRefreshStation :: MsgsRead ThinkLevelMsgsPhase m => RoomItemThink RefreshStationData m
thinkRefreshStation refreshStation =
    let
        interactPressed :: [PlayerMsgPayload] -> Bool
        interactPressed []     = False
        interactPressed (d:ds) = case d of
            PlayerMsgInteract _ -> True
            _                   -> interactPressed ds
    in (interactPressed <$> readMsgs) <&> \case
        True
            | isRefreshStationFilled refreshStation && _touchingPlayer (_data refreshStation) ->
                let
                    pos = hitboxBotCenter $ _hitbox refreshStation

                    setOverlaySpr = \rs ->
                        let rsData = _data rs
                        in rs {_data = rsData {_overlaySprite = _meterDrain $ _sprites rsData}}
                in
                    [ mkMsg PlayerMsgFillMeterFull
                    , mkMsg UiMsgFullRefillMeter
                    , mkMsgTo (RoomMsgUpdateItem setOverlaySpr) (_msgId refreshStation)
                    , mkMsg $ AudioMsgPlaySound drainSoundPath pos
                    ]

        _ -> []

updateRefreshStation
    :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsReadWrite UpdateLevelMsgsPhase m)
    => RoomItemUpdate RefreshStationData m
updateRefreshStation refreshStation =
    let
        refreshStationData = _data refreshStation
        overlaySpr         = _overlaySprite refreshStationData
        sprs               = _sprites refreshStationData
        meterFillSpr       = _meterFill sprs

        overlaySpr'
            | overlaySpr == _meterDrain sprs && spriteFinished overlaySpr = meterFillSpr
            | otherwise                                                   = updateSprite overlaySpr
    in do
        when (overlaySpr' == meterFillSpr && overlaySpr /= meterFillSpr) $
            let pos = hitboxBotCenter $ _hitbox refreshStation
            in writeMsgs [mkMsg $ AudioMsgPlaySound refillSoundPath pos]

        overlayInputDisplayTxt <- updateInputDisplayText $ _overlayInputDisplayText refreshStationData

        return $ refreshStation
            { _data = refreshStationData
                { _touchingPlayer          = False
                , _overlaySprite           = overlaySpr'
                , _overlayInputDisplayText = overlayInputDisplayTxt
                }
            }

drawOverlay :: (GraphicsReadWrite m, InputRead m, MonadIO m) => Pos2 -> InputDisplayText -> m ()
drawOverlay (Pos2 x y) inputDisplayTxt = do
    rectWidth <- (+ (overlayBackdropBorderSize * 2.0)) <$> inputDisplayTextWidth inputDisplayTxt
    let
        rectX   = x - rectWidth / 2.0
        rectPos = Pos2 rectX (y + overlayBackdropOffsetY)
    drawRect rectPos rectWidth overlayBackdropHeight overlayBackdropColor uiInfoTextZIndex

    let textPos = Pos2 x (y + overlayTextCenterOffsetY)
    drawInputDisplayTextCentered textPos uiInfoTextZIndex inputDisplayTxt

drawRefreshStation :: (FileCache m, GraphicsReadWrite m, InputRead m, MonadIO m) => RoomItemDraw RefreshStationData m
drawRefreshStation refreshStation =
    let
        imgPos             = hitboxBotCenter $ _hitbox refreshStation
        sprPos             = imgPos `vecAdd` Pos2 0 refreshStationOverlayOffsetY
        refreshStationData = _data refreshStation
        img                = _image (refreshStationData :: RefreshStationData)
        overlaySpr         = _overlaySprite refreshStationData
    in do
        drawImage imgPos RightDir levelItemZIndex img
        drawSprite sprPos RightDir levelItemZIndex overlaySpr

        when (isRefreshStationFilled refreshStation && _touchingPlayer refreshStationData) $
            drawOverlay imgPos (_overlayInputDisplayText refreshStationData)

refreshStationPlayerCollision :: RoomItemPlayerCollision RefreshStationData
refreshStationPlayerCollision _ refreshStation = [mkMsgTo (RoomMsgUpdateItem updateTouching) (_msgId refreshStation)]
    where updateTouching = \rs -> rs {_data = (_data rs) {_touchingPlayer = True}}
