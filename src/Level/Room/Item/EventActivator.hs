module Level.Room.Item.EventActivator
    ( mkEventActivator
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.Text as T

import Collision.Hitbox
import Configs
import FileCache
import Id
import Level.Room.Event.BouncingBall
import Level.Room.Event.LightningStrike
import Level.Room.Event.SlotMachine
import Level.Room.Event.Types
import Level.Room.Item as RI
import Level.Room.Item.EventActivator.Types
import Msg
import Particle.All.Simple
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

eventActivatorWidth  = 86.0  :: Float
eventActivatorHeight = 315.0 :: Float

interactText                      = "Begin: {InteractAlias}"              :: T.Text
instructionTextColor              = Color 192 192 192 255                 :: Color
interactOverlayBackdropColor      = Color 0 0 0 200                       :: Color
interactOverlayBackdropBorderSize = 20.0                                  :: Float
interactOverlayBackdropOffsetY    = -245.0                                :: OffsetY
interactOverlayBackdropHeight     = 85.0                                  :: Float
interactOverlayText0OffsetY       = interactOverlayBackdropOffsetY + 18.0 :: OffsetY
interactOverlayText1OffsetY       = interactOverlayText0OffsetY + 44.0    :: OffsetY

packPath           = \f -> PackResourceFilePath "data/levels/level-items.pack" f
inactiveSpritePath = packPath "event-sign-inactive.spr"        :: PackResourceFilePath
activateSoundPath  = "event:/SFX Events/Level/event-activator" :: FilePath

eventTypeToInstructionText :: RoomEventType -> T.Text
eventTypeToInstructionText = \case
    BouncingBallEvent    -> "| Hit Bouncing Ball |"
    LightningStrikeEvent -> "| Dodge Lightning |"
    SlotMachineEvent     -> ""

eventTypeToImagePath :: RoomEventType -> PackResourceFilePath
eventTypeToImagePath = packPath . \case
    BouncingBallEvent    -> "bouncing-ball-sign.image"
    LightningStrikeEvent -> "lightning-strike-sign.image"
    SlotMachineEvent     -> ""

mkEventActivatorData :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m) => RoomEventType -> m EventActivatorData
mkEventActivatorData eventType = do
    img             <- loadPackImage $ eventTypeToImagePath eventType
    displayTxt      <- mkDisplayText (eventTypeToInstructionText eventType) Font29 instructionTextColor
    inputDisplayTxt <- mkInputDisplayText interactText Font32 whiteColor

    return $ EventActivatorData
        { _eventType        = eventType
        , _touchingPlayer   = False
        , _image            = img
        , _displayText      = displayTxt
        , _inputDisplayText = inputDisplayTxt
        }

mkEventActivator
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => RoomEventType
    -> Pos2
    -> m (Some RoomItem)
mkEventActivator SlotMachineEvent pos = mkRoomEventSlotMachine pos
mkEventActivator eventType (Pos2 x y) =
    let
        pos = Pos2 (x - eventActivatorWidth / 2.0) (y - eventActivatorHeight)
        hbx = rectHitbox pos eventActivatorWidth eventActivatorHeight
    in do
        eventActivatorData <- mkEventActivatorData eventType
        msgId              <- newId

        return . Some $ (mkRoomItem EventActivatorItemType eventActivatorData msgId hbx)
            { _draw            = drawEventActivator
            , _think           = thinkEventActivator
            , _update          = updateEventActivator
            , _playerCollision = eventActivatorPlayerCollision
            , _inInteractRange = _touchingPlayer . _data
            }

thinkEventActivator :: MsgsRead ThinkLevelMsgsPhase m => RoomItemThink EventActivatorData m
thinkEventActivator eventActivator =
    let
        interactPressed :: [PlayerMsgPayload] -> Bool
        interactPressed []     = False
        interactPressed (d:ds) = case d of
            PlayerMsgInteract _ -> True
            _                   -> interactPressed ds

        eventActivatorData = _data eventActivator
        hbx                = RI._hitbox eventActivator
        hbxCenter          = hitboxCenter hbx
        hbxBotCenter       = hitboxBotCenter hbx

        mkInactiveEventActivatorParticle = loadSimpleParticle hbxBotCenter RightDir levelItemZIndex inactiveSpritePath
    in do
        isPressed <- interactPressed <$> readMsgs
        return $ if
            | isPressed && _touchingPlayer eventActivatorData ->
                let
                    eventMsg = case _eventType eventActivatorData of
                        BouncingBallEvent    -> mkMsg $ EnemyMsgAddM (mkRoomEventBouncingBall hbxCenter)
                        LightningStrikeEvent ->
                            mkMsg $ NewThinkProjectileMsgAddM (mkRoomEventLightningStrike hbxCenter)
                        SlotMachineEvent     -> mkMsg $ ConsoleMsgPrint "???"
                in
                    [ eventMsg
                    , mkMsg $ AudioMsgPlaySound activateSoundPath hbxCenter
                    , mkMsg $ RoomMsgRemoveItem (_msgId eventActivator)
                    , mkMsg $ ParticleMsgAddM mkInactiveEventActivatorParticle
                    , mkMsg RoomMsgAddPortalBarrier
                    ]

            | otherwise -> []

updateEventActivator :: MsgsReadWrite UpdateLevelMsgsPhase m => RoomItemUpdate EventActivatorData m
updateEventActivator eventActivator = return $ eventActivator
    { _data = (_data eventActivator) {_touchingPlayer = False}
    }

drawInteractOverlay :: (GraphicsReadWrite m, InputRead m, MonadIO m) => RoomItem EventActivatorData -> m ()
drawInteractOverlay eventActivator = do
    let
        eventActivatorData = _data eventActivator
        inputDisplayTxt    = _inputDisplayText eventActivatorData
        displayTxt         = _displayText eventActivatorData

    widths <- sequenceA
        [ inputDisplayTextWidth inputDisplayTxt
        , displayTextWidth displayTxt
        ]

    let
        Pos2 x y  = hitboxCenter $ RI._hitbox eventActivator
        rectWidth = fromMaybe 0.0 (maybeMaximum widths) + interactOverlayBackdropBorderSize * 2.0
        rectX     = x - rectWidth / 2.0
        rectPos   = Pos2 rectX (y + interactOverlayBackdropOffsetY)
    drawRect rectPos rectWidth interactOverlayBackdropHeight interactOverlayBackdropColor uiInfoTextZIndex

    let
        textPos0 = Pos2 x (y + interactOverlayText0OffsetY)
        textPos1 = Pos2 x (y + interactOverlayText1OffsetY)
    drawDisplayTextCentered textPos0 uiInfoTextZIndex displayTxt
    drawInputDisplayTextCentered textPos1 uiInfoTextZIndex inputDisplayTxt

drawEventActivator :: (FileCache m, GraphicsReadWrite m, InputRead m, MonadIO m) => RoomItemDraw EventActivatorData m
drawEventActivator eventActivator =
    let
        imgPos             = hitboxBotCenter $ _hitbox eventActivator
        eventActivatorData = _data eventActivator
        img                = _image (eventActivatorData :: EventActivatorData)
    in do
        drawImage imgPos RightDir levelItemZIndex img

        when (_touchingPlayer eventActivatorData) $
            drawInteractOverlay eventActivator

eventActivatorPlayerCollision :: RoomItemPlayerCollision EventActivatorData
eventActivatorPlayerCollision _ eventActivator = [mkMsgTo (RoomMsgUpdateItem updateTouching) (_msgId eventActivator)]
    where updateTouching = \ea -> ea {_data = (_data ea) {_touchingPlayer = True}}
