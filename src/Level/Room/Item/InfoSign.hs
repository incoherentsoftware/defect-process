module Level.Room.Item.InfoSign
    ( mkInfoSign
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T

import Collision.Hitbox
import FileCache
import Id
import Level.Room.Item
import Level.Room.Item.InfoSign.Types
import Msg
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

infoSignWidth  = 150.0  :: Float
infoSignHeight = 1000.0 :: Float

overlayText               = "View equipment and help: {MenuAlias}"               :: T.Text
overlayBackdropColor      = Color 0 0 0 200                                      :: Color
overlayBackdropBorderSize = 20.0                                                 :: Float
overlayBackdropOffsetY    = -415.0                                               :: PosY
overlayBackdropHeight     = 60.0                                                 :: Float
overlayTextCenterOffsetY  = overlayBackdropOffsetY + overlayBackdropHeight / 2.0 :: PosY

roomImagePath = PackResourceFilePath "data/levels/level-items.pack" "info-sign.image" :: PackResourceFilePath

mkInfoSignData :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m) => m InfoSignData
mkInfoSignData = do
    img                    <- loadPackImage roomImagePath
    overlayInputDisplayTxt <- mkInputDisplayText overlayText Font32 whiteColor

    return $ InfoSignData
        { _touchingPlayer          = False
        , _image                   = img
        , _overlayInputDisplayText = overlayInputDisplayTxt
        }

mkInfoSign :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m) => Pos2 -> m (Some RoomItem)
mkInfoSign (Pos2 x y) =
    let
        x'  = x - infoSignWidth / 2.0
        y'  = y - infoSignHeight
        hbx = rectHitbox (Pos2 x' y') infoSignWidth infoSignHeight
    in do
        infoSignData <- mkInfoSignData
        msgId        <- newId
        return . Some $ (mkRoomItem InfoSignItemType infoSignData msgId hbx)
            { _draw            = drawInfoSign
            , _update          = updateInfoSign
            , _playerCollision = infoSignPlayerCollision
            }

updateInfoSign :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m) => RoomItemUpdate InfoSignData m
updateInfoSign infoSign = do
    let infoSignData        = _data infoSign
    overlayInputDisplayTxt <- updateInputDisplayText $ _overlayInputDisplayText infoSignData

    return $ infoSign
        { _data = infoSignData
            { _touchingPlayer          = False
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

drawInfoSign :: (FileCache m, GraphicsReadWrite m, InputRead m, MonadIO m) => RoomItemDraw InfoSignData m
drawInfoSign infoSign =
    let
        pos          = hitboxBotCenter $ _hitbox infoSign
        infoSignData = _data infoSign
        img          = _image (infoSignData :: InfoSignData)
    in do
        drawImage pos RightDir levelItemZIndex img
        when (_touchingPlayer infoSignData) $
            drawOverlay pos (_overlayInputDisplayText infoSignData)

infoSignPlayerCollision :: RoomItemPlayerCollision InfoSignData
infoSignPlayerCollision _ infoSign =
    [ mkMsgTo (RoomMsgUpdateItem updateTouching) (_msgId infoSign)
    , mkMsg PlayerMsgTouchingInfoSign
    ]
    where updateTouching = \s -> s {_data = (_data s) {_touchingPlayer = True}}
