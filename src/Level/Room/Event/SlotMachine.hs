module Level.Room.Event.SlotMachine
    ( mkRoomEventSlotMachine
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (for_)
import Data.Maybe             (listToMaybe)
import Data.Traversable       (for)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Collision.Hitbox
import Configs
import Configs.All.Level
import Constants
import FileCache
import Id
import Level.Room.Event.SlotMachine.TextParticle
import Level.Room.Event.SlotMachine.Util
import Level.Room.Item as RI
import Msg
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

signWidth              = 86.0        :: Float
signHeight             = 315.0       :: Float
doneNotSelectedOpacity = Opacity 0.5 :: Opacity

interactText                      = "Gamble: {InteractAlias}"             :: T.Text
interactOverlayBackdropColor      = Color 0 0 0 200                       :: Color
interactOverlayBackdropBorderSize = 20.0                                  :: Float
interactOverlayBackdropOffsetY    = -247.0                                :: OffsetY
interactOverlayBackdropHeight     = 47.0                                  :: Float
interactOverlayTextOffsetY        = interactOverlayBackdropOffsetY + 24.0 :: OffsetY

packPath           = \f -> PackResourceFilePath "data/levels/level-items.pack" f
signImagePath      = packPath "slot-machine-sign.image"      :: PackResourceFilePath
selectionImagePath = packPath "slot-machine-selection.image" :: PackResourceFilePath

activateSoundPath = "event:/SFX Events/Level/event-activator" :: FilePath
goldSoundPath     = "event:/SFX Events/Level/gold-pickup"     :: FilePath

data SlotMachineState
    = SlotMachineReady
    | SlotMachineActivated
    | SlotMachineDone
    deriving Eq

data SlotMachineData = SlotMachineData
    { _state                 :: SlotMachineState
    , _touchingPlayer        :: Bool
    , _selectionTtl          :: Secs
    , _selectionIntervalSecs :: Secs
    , _selectionOffset       :: Pos2
    , _signImage             :: Image
    , _selectionImage        :: Image
    , _symbolDisplayTexts    :: [SymbolDisplayText]
    , _inputDisplayText      :: InputDisplayText
    , _config                :: LevelConfig
    }

chooseSlotsChoices :: (ConfigsRead m, MonadIO m) => m [SlotsChoice]
chooseSlotsChoices = do
    cfg                 <- _level <$> readConfigs
    let selectionOffsets = _eventSlotMachineSelectionOffsets cfg
    slotsChoices        <- for [0..length selectionOffsets - 1] $ \_ ->
        randomChoice $ _eventSlotMachineSlotsChoices cfg

    return $ if
        | isSlotsChoicesAllPlus slotsChoices  -> Minus100PercentChoice:safeTail slotsChoices
        | isSlotsChoicesAllMinus slotsChoices -> Plus100PercentChoice:safeTail slotsChoices
        | otherwise                           -> slotsChoices

mkSlotMachineData :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m) => m SlotMachineData
mkSlotMachineData = do
    signImg         <- loadPackImage signImagePath
    selectionImg    <- loadPackImage selectionImagePath
    inputDisplayTxt <- mkInputDisplayText interactText Font32 whiteColor
    cfg             <- _level <$> readConfigs

    selectionOffset   <- randomChoice $ _eventSlotMachineSelectionOffsets cfg
    slotsChoices      <- chooseSlotsChoices
    symbolDisplayTxts <- for slotsChoices $ \sc ->
        let txt = formatSlotsChoice sc
        in mkSymbolDisplayText txt Font32 (slotsChoiceTextColor txt)

    return $ SlotMachineData
        { _state                 = SlotMachineReady
        , _touchingPlayer        = False
        , _selectionTtl          = _eventSlotMachineSelectionIntervalSecs cfg
        , _selectionIntervalSecs = _eventSlotMachineSelectionIntervalSecs cfg
        , _selectionOffset       = selectionOffset
        , _signImage             = signImg
        , _selectionImage        = selectionImg
        , _symbolDisplayTexts    = symbolDisplayTxts
        , _inputDisplayText      = inputDisplayTxt
        , _config                = cfg
        }

mkRoomEventSlotMachine
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m)
    => Pos2
    -> m (Some RoomItem)
mkRoomEventSlotMachine (Pos2 x y) =
    let
        pos = Pos2 (x - signWidth / 2.0) (y - signHeight)
        hbx = rectHitbox pos signWidth signHeight
    in do
        slotMachineData <- mkSlotMachineData
        msgId           <- newId

        return . Some $ (mkRoomItem EventActivatorItemType slotMachineData msgId hbx)
            { _draw            = drawSlotMachine
            , _think           = thinkSlotMachine
            , _update          = updateSlotMachine
            , _playerCollision = slotMachinePlayerCollision
            }

pos2ApproxEq :: Pos2 -> Pos2 -> Bool
pos2ApproxEq (Pos2 x1 y1) (Pos2 x2 y2) = x1 `approxEq` x2 && y1 `approxEq` y2

readSelectionSymbolDisplayText :: SlotMachineData -> Maybe SymbolDisplayText
readSelectionSymbolDisplayText slotMachineData = listToMaybe
    [ symbolDisplayTxt
    | (offset, symbolDisplayTxt) <- zip selectionOffsets (_symbolDisplayTexts slotMachineData)
    , offset `pos2ApproxEq` _selectionOffset slotMachineData
    ]
    where selectionOffsets = NE.toList $ _eventSlotMachineSelectionOffsets (_config slotMachineData)

thinkSlotMachine :: MsgsRead ThinkLevelMsgsPhase m => RoomItemThink SlotMachineData m
thinkSlotMachine slotMachine =
    let
        interactPressed :: [PlayerMsgPayload] -> Bool
        interactPressed []     = False
        interactPressed (d:ds) = case d of
            PlayerMsgInteract _ -> True
            _                   -> interactPressed ds

        setSlotMachineActivated :: RoomItem SlotMachineData -> RoomItem SlotMachineData
        setSlotMachineActivated sm =
            let
                smData                = _data sm
                selectionIntervalSecs = _eventSlotMachineSelectionActivateIntervalSecs $ _config smData
            in sm
                { _data = smData
                    { _state                 = SlotMachineActivated
                    , _selectionTtl          = 0.0
                    , _selectionIntervalSecs = selectionIntervalSecs
                    }
                }

        slotMachineData          = _data slotMachine
        selectionMinIntervalSecs = _eventSlotMachineSelectionMinIntervalSecs $ _config slotMachineData
        pos                      = hitboxCenter $ RI._hitbox slotMachine
    in do
        isPressed <- interactPressed <$> readMsgs
        return $ case _state slotMachineData of
            SlotMachineReady
                | isPressed && _touchingPlayer slotMachineData ->
                    [ mkMsgTo (RoomMsgUpdateItem setSlotMachineActivated) (_msgId slotMachine)
                    , mkMsg RoomMsgAddPortalBarrier
                    , mkMsg $ AudioMsgPlaySound activateSoundPath pos
                    ]

            SlotMachineActivated
                | _selectionIntervalSecs slotMachineData > selectionMinIntervalSecs       ->
                    [mkMsg RoomMsgAddPortalBarrier]
                | Just symbolDisplayTxt <- readSelectionSymbolDisplayText slotMachineData ->
                    let
                        setSlotMachineDone = \sm -> sm {_data = (_data sm) {_state = SlotMachineDone}}
                        selectionTxt       = _text (symbolDisplayTxt :: SymbolDisplayText)
                    in
                        [ mkMsgTo (RoomMsgUpdateItem setSlotMachineDone) (_msgId slotMachine)
                        , mkMsg $ PlayerMsgUpdateGold (parseFormattedSlotsChoice selectionTxt)
                        , mkMsg $ ParticleMsgAddM (mkSlotMachineTextParticle symbolDisplayTxt)
                        , mkMsg $ AudioMsgPlaySound goldSoundPath pos
                        ]

            _ -> []

chooseRandomSelectionOffset :: MonadIO m => SlotMachineData -> m Pos2
chooseRandomSelectionOffset slotMachineData = case NE.filter (not . pos2ApproxEq selectionOffset) selectionOffsets of
    []      -> return selectionOffset
    offsets -> randomChoice $ NE.fromList offsets
    where
        selectionOffset  = _selectionOffset slotMachineData
        selectionOffsets = _eventSlotMachineSelectionOffsets $ _config slotMachineData

updateSlotMachine :: MonadIO m => RoomItemUpdate SlotMachineData m
updateSlotMachine slotMachine = do
    let
        slotMachineData = _data slotMachine
        selectionTtl    = _selectionTtl slotMachineData - timeStep
        state           = _state slotMachineData

    selectionOffset <- if
        | state /= SlotMachineDone && selectionTtl <= 0.0 -> chooseRandomSelectionOffset slotMachineData
        | otherwise                                       -> return $ _selectionOffset slotMachineData

    let
        isActivated                 = state == SlotMachineActivated
        selectionIntervalSecs       = _selectionIntervalSecs slotMachineData
        cfg                         = _config slotMachineData
        selectionIntervalMultiplier = _eventSlotMachineSelectionActivateIntervalMultiplier cfg

        selectionIntervalSecs'
            | isActivated && selectionTtl <= 0.0 = selectionIntervalSecs * selectionIntervalMultiplier
            | otherwise                          = selectionIntervalSecs

        selectionTtl'
            | selectionTtl <= 0.0 = selectionIntervalSecs'
            | otherwise           = selectionTtl

    return $ slotMachine
        { _data = slotMachineData
            { _touchingPlayer        = False
            , _selectionTtl          = selectionTtl'
            , _selectionIntervalSecs = selectionIntervalSecs'
            , _selectionOffset       = selectionOffset
            }
        }

drawInteractOverlay :: (GraphicsReadWrite m, InputRead m, MonadIO m) => RoomItem SlotMachineData -> m ()
drawInteractOverlay slotMachine = do
    let
        slotMachineData = _data slotMachine
        inputDisplayTxt = _inputDisplayText slotMachineData
    inputDisplayTxtWidth <- inputDisplayTextWidth inputDisplayTxt

    let
        Pos2 x y  = hitboxCenter $ RI._hitbox slotMachine
        rectWidth = inputDisplayTxtWidth + interactOverlayBackdropBorderSize * 2.0
        rectX     = x - rectWidth / 2.0
        rectPos   = Pos2 rectX (y + interactOverlayBackdropOffsetY)
    drawRect rectPos rectWidth interactOverlayBackdropHeight interactOverlayBackdropColor uiInfoTextZIndex

    let textPos = Pos2 x (y + interactOverlayTextOffsetY)
    drawInputDisplayTextCentered textPos uiInfoTextZIndex inputDisplayTxt

symbolDisplayTextOpacity :: Pos2 -> SlotMachineData -> Opacity
symbolDisplayTextOpacity offset slotMachineData = case _state slotMachineData of
    SlotMachineDone
        | not (offset `pos2ApproxEq` _selectionOffset slotMachineData) -> doneNotSelectedOpacity
    _                                                                  -> FullOpacity

drawSlotMachine :: (GraphicsReadWrite m, InputRead m, MonadIO m) => RoomItemDraw SlotMachineData m
drawSlotMachine slotMachine =
    let
        signImgPos        = hitboxBotCenter $ _hitbox slotMachine
        slotMachineData   = _data slotMachine
        selectionOffsets  = NE.toList $ _eventSlotMachineSelectionOffsets (_config slotMachineData)
        symbolDisplayTxts = _symbolDisplayTexts slotMachineData
    in do
        drawImage signImgPos RightDir levelItemZIndex (_signImage slotMachineData)
        for_ (zip selectionOffsets symbolDisplayTxts) $ \(selectionOffset, symbolDisplayTxt) ->
            let
                textOffset = _eventSlotMachineSelectionTextOffset $ _config slotMachineData
                pos        = signImgPos `vecAdd` selectionOffset `vecAdd` textOffset
                opacity    = symbolDisplayTextOpacity selectionOffset slotMachineData
            in drawSymbolDisplayTextCenteredEx pos levelItemZIndex NonScaled opacity symbolDisplayTxt

        let selectionPos = signImgPos `vecAdd` _selectionOffset slotMachineData
        drawImage selectionPos RightDir levelItemZIndex (_selectionImage slotMachineData)

        when (_state slotMachineData == SlotMachineReady && _touchingPlayer slotMachineData) $
            drawInteractOverlay slotMachine

slotMachinePlayerCollision :: RoomItemPlayerCollision SlotMachineData
slotMachinePlayerCollision _ slotMachine = [mkMsgTo (RoomMsgUpdateItem updateTouching) (_msgId slotMachine)]
    where updateTouching = \sm -> sm {_data = (_data sm) {_touchingPlayer = True}}
