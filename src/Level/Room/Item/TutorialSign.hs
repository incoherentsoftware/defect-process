module Level.Room.Item.TutorialSign
    ( mkTutorialSign
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (traverse_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Async.Request
import Collision.Hitbox
import FileCache
import Id
import Level.Room.Item
import Level.Room.Item.TutorialSign.Types
import Level.Room.Util
import Msg
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

allTutorialPreloadPackFilePaths =
    [ "data/levels/level-tutorial.pack"
    , "data/particles/particles.pack"
    , "data/particles/particles-enemy.pack"
    ] :: [FilePath]

tutorialSignWidth  = 150.0  :: Float
tutorialSignHeight = 1000.0 :: Float

overlayLine0Text          = "Lock-on Targeting"           :: T.Text
overlayLine1Text          = "Tutorial: {InteractAlias}"   :: T.Text
overlayBackdropColor      = Color 0 0 0 200               :: Color
overlayBackdropBorderSize = 20.0                          :: Float
overlayBackdropOffsetY    = -466.0                        :: PosY
overlayBackdropHeight     = 99.0                          :: Float
overlayLine0TextOffsetY   = overlayBackdropOffsetY + 30.0 :: PosY
overlayLine1TextOffsetY   = overlayBackdropOffsetY + 72.0 :: PosY

roomImagePath = PackResourceFilePath "data/levels/level-items.pack" "tutorial-sign.image" :: PackResourceFilePath

mkTutorialSignData :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m) => m TutorialSignData
mkTutorialSignData = do
    img                         <- loadPackImage roomImagePath
    overlayLine0DisplayTxt      <- mkDisplayText overlayLine0Text Font32 whiteColor
    overlayLine1InputDisplayTxt <- mkInputDisplayText overlayLine1Text Font32 whiteColor

    return $ TutorialSignData
        { _touchingPlayer               = False
        , _image                        = img
        , _overlayLine0DisplayText      = overlayLine0DisplayTxt
        , _overlayLine1InputDisplayText = overlayLine1InputDisplayTxt
        }

mkTutorialSign :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m) => Pos2 -> m (Some RoomItem)
mkTutorialSign (Pos2 x y) =
    let
        x'  = x - tutorialSignWidth / 2.0
        y'  = y - tutorialSignHeight
        hbx = rectHitbox (Pos2 x' y') tutorialSignWidth tutorialSignHeight
    in do
        tutorialSignData <- mkTutorialSignData
        msgId            <- newId
        return . Some $ (mkRoomItem TutorialSignItemType tutorialSignData msgId hbx)
            { _draw            = drawTutorialSign
            , _update          = updateTutorialSign
            , _playerCollision = tutorialSignPlayerCollision
            , _inInteractRange = _touchingPlayer . _data
            }

updateTutorialSign
    :: (AsyncRequestWrite m, FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsWrite UpdateLevelMsgsPhase m)
    => RoomItemUpdate TutorialSignData m
updateTutorialSign tutorialSign = do
    let tutorialSignData = _data tutorialSign
    inputState          <- readInputState

    when (_touchingPlayer tutorialSignData && InteractAlias `aliasPressed` inputState) $ do
        traverse_ (writeAsyncRequest . PreloadPackFileRequest) allTutorialPreloadPackFilePaths
        writeMsgs
            [ mkMsg WorldMsgSaveRoomItems
            , mkMsg $ WorldMsgSwitchRoom tutorialRoomType 0.0
            ]

    overlayLine1InputDisplayTxt <- updateInputDisplayText $ _overlayLine1InputDisplayText tutorialSignData

    return $ tutorialSign
        { _data = tutorialSignData
            { _touchingPlayer               = False
            , _overlayLine1InputDisplayText = overlayLine1InputDisplayTxt
            }
        }

drawOverlay :: (GraphicsReadWrite m, InputRead m, MonadIO m) => Pos2 -> TutorialSignData -> m ()
drawOverlay (Pos2 x y) tutorialSignData = do
    let
        line0DisplayTxt      = _overlayLine0DisplayText tutorialSignData
        line1InputDisplayTxt = _overlayLine1InputDisplayText tutorialSignData

    maxWidth <- fmap maximum . sequenceA $ NE.fromList
        [ displayTextWidth line0DisplayTxt
        , inputDisplayTextWidth line1InputDisplayTxt
        ]

    let
        rectWidth = maxWidth + overlayBackdropBorderSize * 2.0
        rectX     = x - rectWidth / 2.0
        rectPos   = Pos2 rectX (y + overlayBackdropOffsetY)
    drawRect rectPos rectWidth overlayBackdropHeight overlayBackdropColor uiInfoTextZIndex

    let
        text0Pos = Pos2 x (y + overlayLine0TextOffsetY)
        text1Pos = Pos2 x (y + overlayLine1TextOffsetY)
    drawDisplayTextCentered text0Pos uiInfoTextZIndex line0DisplayTxt
    drawInputDisplayTextCentered text1Pos uiInfoTextZIndex line1InputDisplayTxt

drawTutorialSign :: (FileCache m, GraphicsReadWrite m, InputRead m, MonadIO m) => RoomItemDraw TutorialSignData m
drawTutorialSign tutorialSign =
    let
        pos              = hitboxBotCenter $ _hitbox tutorialSign
        tutorialSignData = _data tutorialSign
        img              = _image (tutorialSignData :: TutorialSignData)
    in do
        drawImage pos RightDir levelItemZIndex img
        when (_touchingPlayer tutorialSignData) $
            drawOverlay pos tutorialSignData

tutorialSignPlayerCollision :: RoomItemPlayerCollision TutorialSignData
tutorialSignPlayerCollision _ tutorialSign = [mkMsgTo (RoomMsgUpdateItem updateTouching) (_msgId tutorialSign)]
    where updateTouching = \s -> s {_data = (_data s) {_touchingPlayer = True}}
