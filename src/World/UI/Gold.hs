module World.UI.Gold
    ( GoldUI
    , mkGoldUI
    , updateGoldUI
    , drawGoldUI
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execState, modify)
import Data.Foldable          (traverse_)
import qualified Data.Text as T

import Configs
import Configs.All.Settings
import Configs.All.Settings.UI
import Constants
import FileCache
import Msg
import Player
import Util
import Window.Graphics
import World.UI.Util
import World.Util
import World.ZIndex

goldBackdropColor                 = Color 0 0 0 128 :: Color
insufficientOverlayFadeMultiplier = 2.0             :: Float
insufficientOverlayOffsetX        = 46.0            :: Float

insufficientOverlayBackdropColor :: Opacity -> Color
insufficientOverlayBackdropColor opacity = Color 0 0 0 (opacityToAlpha opacity)

formatGoldSymbolText :: GoldValue -> T.Text
formatGoldSymbolText gold = "{GoldSymbol} " <> prettyShow gold

formatGoldFloatingTextValue :: GoldValue -> T.Text
formatGoldFloatingTextValue gold = "+" <> prettyShow gold

data GoldFloatingText = GoldFloatingText
    { _displayText :: DisplayText
    , _offset      :: Pos2
    , _opacity     :: Opacity
    }

mkGoldFloatingText :: GoldValue -> GoldUI -> UiConfig -> GoldFloatingText
mkGoldFloatingText goldValue goldUI cfg = GoldFloatingText
    { _displayText = updateDisplayText txt (_goldDropDisplayText goldUI)
    , _offset      = _goldFloatingTextOffset cfg
    , _opacity     = FullOpacity
    }
    where
        txt
            | goldValue > GoldValue 0 = formatGoldFloatingTextValue goldValue
            | otherwise               = ""

updateGoldFloatingText :: UiConfig -> GoldFloatingText -> GoldFloatingText
updateGoldFloatingText cfg floatingText = floatingText
    { _offset  = offset
    , _opacity = opacity
    }
    where
        offset  = _offset floatingText `vecAdd` toPos2 (Vel2 0.0 (_goldFloatingTextVelY cfg * timeStep))
        opacity = decreaseOpacity (_goldFloatingTextFadeMultiplier cfg * timeStep) (_opacity floatingText)

drawGoldFloatingText :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> DrawScale -> GoldFloatingText -> m ()
drawGoldFloatingText pos drawScale floatingText =
    drawDisplayTextRightAlignedEx pos' uiFrontZIndex drawScale opacity displayText
    where
        offset      = _offset floatingText `vecMul` drawScaleToFloat drawScale
        pos'        = pos `vecAdd` offset
        opacity     = _opacity floatingText
        displayText = _displayText floatingText

data GoldUI = GoldUI
    { _symbolDisplayText          :: SymbolDisplayText
    , _goldDropDisplayText        :: DisplayText
    , _goldFloatingTexts          :: [GoldFloatingText]
    , _goldFloatingTextsQueue     :: [GoldFloatingText]  -- should be deque but size is small so doesn't matter
    , _goldFloatingTextCooldown   :: Secs
    , _insufficientOverlayImage   :: Image
    , _insufficientOverlayOpacity :: Opacity
    }

mkGoldUI :: (FileCache m, GraphicsRead m, MonadIO m) => m GoldUI
mkGoldUI = do
    symbolDisplayTxt       <- mkSymbolDisplayText (formatGoldSymbolText $ GoldValue 0) Font32 goldTextColor
    goldDropDisplayText    <- mkDisplayText (formatGoldFloatingTextValue (GoldValue 0)) Font32 goldTextColor
    insufficientOverlayImg <- loadUiPackImage "insufficient-gold-overlay.image"

    return $ GoldUI
        { _symbolDisplayText          = symbolDisplayTxt
        , _goldDropDisplayText        = goldDropDisplayText
        , _goldFloatingTexts          = []
        , _goldFloatingTextsQueue     = []
        , _goldFloatingTextCooldown   = 0.0
        , _insufficientOverlayImage   = insufficientOverlayImg
        , _insufficientOverlayOpacity = Opacity 0.0
        }

updateGoldUI
    :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m, MsgsRead UpdateWorldUiMsgsPhase m)
    => Player
    -> GoldUI
    -> m GoldUI
updateGoldUI player goldUI = do
    cfg <- readConfig _settings _ui

    let
        processOpacityMsgs :: [UiMsgPayload] -> Opacity
        processOpacityMsgs []     =
            decreaseOpacity (insufficientOverlayFadeMultiplier * timeStep) (_insufficientOverlayOpacity goldUI)
        processOpacityMsgs (p:ps) = case p of
            UiMsgInsufficientGold -> Opacity 1.0
            _                     -> processOpacityMsgs ps

        processGainedGoldMsgs :: UiMsgPayload -> [GoldFloatingText] -> [GoldFloatingText]
        processGainedGoldMsgs p floatingTexts = case p of
            UiMsgGainedGold goldValue -> floatingTexts ++ [mkGoldFloatingText goldValue goldUI cfg]
            _                         -> floatingTexts

    let txt           = formatGoldSymbolText (_gold player :: GoldValue)
    symbolDisplayTxt <- updateSymbolDisplayText txt (_symbolDisplayText goldUI)

    insufficientOverlayOpacity <- processOpacityMsgs <$> readMsgs
    goldFloatingTextsQueue     <- foldr processGainedGoldMsgs (_goldFloatingTextsQueue goldUI) <$> readMsgs

    return . flip execState goldUI $ do
        modify $ \ui -> ui
            { _symbolDisplayText          = symbolDisplayTxt
            , _insufficientOverlayOpacity = insufficientOverlayOpacity
            , _goldFloatingTextCooldown   = max 0.0 (_goldFloatingTextCooldown ui - timeStep)
            }

        modify $ \ui -> case goldFloatingTextsQueue of
            (gft:gfts)
                | _goldFloatingTextCooldown ui <= 0.0 -> ui
                    { _goldFloatingTexts        = gft:_goldFloatingTexts ui
                    , _goldFloatingTextsQueue   = gfts
                    , _goldFloatingTextCooldown = _goldFloatingTextIntervalSecs cfg
                    }
            _                                         -> ui {_goldFloatingTextsQueue = goldFloatingTextsQueue}

        modify $ \ui -> ui
            { _goldFloatingTexts = map (updateGoldFloatingText cfg) (_goldFloatingTexts ui)
            }
        modify $ \ui -> ui
            { _goldFloatingTexts = filter ((> minAlpha) . opacityToAlpha . _opacity) (_goldFloatingTexts ui)
            }

drawGoldUI :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => GoldUI -> m ()
drawGoldUI goldUI = do
    cfg <- readConfig _settings _ui
    let
        drawScale = _overlayScale cfg
        scale     = drawScaleToFloat drawScale

    let symbolDisplayTxt = _symbolDisplayText goldUI
    symbolTxtWidth      <- symbolDisplayTextWidth symbolDisplayTxt
    symbolTxtHeight     <- symbolDisplayTextHeight symbolDisplayTxt
    let
        goldRightPos  = _goldRightPos cfg `vecMul` scale
        goldCenterPos = goldRightPos `vecAdd` Pos2 (-symbolTxtWidth / 2.0) (symbolTxtHeight / 2.0)

    let
        backdropWidth  = symbolTxtWidth + _goldBackdropBorderWidth cfg * 2.0 * scale
        backdropHeight = _goldBackdropHeight cfg * scale
        backdropPos    = goldCenterPos `vecSub` Pos2 (backdropWidth / 2.0) (backdropHeight / 2.0)
    drawRect backdropPos backdropWidth backdropHeight goldBackdropColor uiFrontZIndex

    drawSymbolDisplayTextRightAlignedEx goldRightPos uiFrontZIndex drawScale FullOpacity symbolDisplayTxt

    let
        insufficientOverlayOpacity = _insufficientOverlayOpacity goldUI
        insufficientOverlayImg     = _insufficientOverlayImage goldUI
        insufficientOverlayColor   = insufficientOverlayBackdropColor insufficientOverlayOpacity
        insufficientOverlayPos     =
            Pos2
            (vecX backdropPos + insufficientOverlayOffsetX + (symbolTxtWidth - insufficientOverlayOffsetX) / 2.0)
            (vecY goldCenterPos)
    drawRect backdropPos backdropWidth backdropHeight insufficientOverlayColor uiFrontZIndex
    drawImageEx
        insufficientOverlayPos
        RightDir
        uiFrontZIndex
        0.0
        insufficientOverlayOpacity
        drawScale
        insufficientOverlayImg

    traverse_ (drawGoldFloatingText goldRightPos drawScale) (_goldFloatingTexts goldUI)
