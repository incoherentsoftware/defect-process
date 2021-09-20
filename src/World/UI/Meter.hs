module World.UI.Meter
    ( MeterUI
    , mkMeterUI
    , updateMeterUI
    , drawMeterUI
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (foldlM, for_, sequenceA_)
import Data.Functor           ((<&>))

import Configs
import Configs.All.Settings
import Configs.All.Settings.UI
import FileCache
import Msg
import Player
import Player.Meter
import Player.Upgrade
import Util
import Window.Graphics
import World.UI.Util
import World.ZIndex

meterMissingSoundPath = "event:/SFX Events/UI/meter-missing" :: FilePath

data MissingMeterFlash = MissingMeterFlash
    { _unitCount                 :: Int
    , _prevInsufficientMeterVals :: [MeterValue]
    , _sprite                    :: Sprite
    }

mkMissingMeterFlash :: Int -> MeterValue -> Sprite -> MissingMeterFlash
mkMissingMeterFlash unitCount meterVal unitMissingSpr = MissingMeterFlash
    { _unitCount                 = unitCount
    , _prevInsufficientMeterVals = [meterVal]
    , _sprite                    = unitMissingSpr
    }

data MeterUiSprites = MeterUiSprites
    { _unitMissing :: Sprite
    , _unitRefill  :: Sprite
    }

mkMeterUiSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m MeterUiSprites
mkMeterUiSprites =
    MeterUiSprites <$>
    loadUiPackSprite "meter-unit-missing.spr" <*>
    loadUiPackSprite "meter-unit-refill.spr"

data MeterUiImages = MeterUiImages
    { _backdropA :: Image
    , _backdropB :: Image
    , _backdropC :: Image
    , _backdropD :: Image
    , _backdropE :: Image
    , _backdropF :: Image
    }

mkMeterUiImages :: (FileCache m, GraphicsRead m, MonadIO m) => m MeterUiImages
mkMeterUiImages =
    MeterUiImages <$>
    loadUiPackImage "meter-backdrop-a.image" <*>
    loadUiPackImage "meter-backdrop-b.image" <*>
    loadUiPackImage "meter-backdrop-c.image" <*>
    loadUiPackImage "meter-backdrop-d.image" <*>
    loadUiPackImage "meter-backdrop-e.image" <*>
    loadUiPackImage "meter-backdrop-f.image"

data MeterUI = MeterUI
    { _missingMeterFlash :: Maybe MissingMeterFlash
    , _unitOverlaySprite :: Maybe Sprite
    , _backdropImage     :: Image
    , _unitFilledImage   :: Image
    , _sprites           :: MeterUiSprites
    , _images            :: MeterUiImages
    }

updateMissingMeterFlash :: MissingMeterFlash -> Maybe MissingMeterFlash
updateMissingMeterFlash mmf
    | sprFinished && noPrevInsufficientMeterVals = Nothing
    | otherwise                                  = Just mmf'
    where
        mmf' = mmf {_sprite = updateSprite $ _sprite (mmf :: MissingMeterFlash)} :: MissingMeterFlash

        sprFinished                 = spriteFinished $ _sprite (mmf' :: MissingMeterFlash)
        noPrevInsufficientMeterVals = null $ _prevInsufficientMeterVals mmf'

mkMeterUI :: (FileCache m, GraphicsRead m, MonadIO m) => m MeterUI
mkMeterUI = do
    sprs          <- mkMeterUiSprites
    imgs          <- mkMeterUiImages
    unitFilledImg <- loadUiPackImage "meter-unit-filled.image"

    return $ MeterUI
        { _missingMeterFlash = Nothing
        , _unitOverlaySprite = Nothing
        , _backdropImage     = _backdropA imgs
        , _unitFilledImage   = unitFilledImg
        , _sprites           = sprs
        , _images            = imgs
        }

processMeterUiMsgs :: MsgsReadWrite UpdateWorldUiMsgsPhase m => Player -> MeterUI -> m MeterUI
processMeterUiMsgs player meterUI = foldlM processMsg (clearPrevInsufficientMeterVals meterUI) =<< readMsgs
    where
        prevInsufficientMeterVals = maybe [] _prevInsufficientMeterVals (_missingMeterFlash meterUI)

        clearPrevInsufficientMeterVals :: MeterUI -> MeterUI
        clearPrevInsufficientMeterVals ui = ui
            { _missingMeterFlash = _missingMeterFlash ui <&> \mmf -> mmf {_prevInsufficientMeterVals = []}
            }

        processMsg :: MsgsWrite UpdateWorldUiMsgsPhase m1 => MeterUI -> UiMsgPayload -> m1 MeterUI
        processMsg ui p = case p of
            UiMsgInsufficientMeter meterVal forceShow
                | playerMeterVal < meterVal ->
                    let
                        unitCount            = _int (meterVal - playerMeterVal :: MeterValue)
                        unitMissingSpr       = _unitMissing $ _sprites (ui :: MeterUI)
                        newMissingMeterFlash = Just $ mkMissingMeterFlash unitCount meterVal unitMissingSpr

                        (missingMeterFlash, isNewOrUpdated) = case _missingMeterFlash ui of
                            _
                                | forceShow -> (newMissingMeterFlash, True)
                            Nothing         -> (newMissingMeterFlash, True)
                            Just mmf        ->
                                let
                                    isUpdated       = meterVal `notElem` prevInsufficientMeterVals
                                    spr
                                        | isUpdated = unitMissingSpr
                                        | otherwise = _sprite (mmf :: MissingMeterFlash)
                                    mmf'            = mmf
                                        { _unitCount                 = max unitCount (_unitCount mmf)
                                        , _prevInsufficientMeterVals = meterVal:_prevInsufficientMeterVals mmf
                                        , _sprite                    = spr
                                        }
                                in (Just mmf', isUpdated)
                    in do
                        when isNewOrUpdated $
                            writeMsgs [mkMsg $ AudioMsgPlaySoundCentered meterMissingSoundPath]
                        return $ ui { _missingMeterFlash = missingMeterFlash}

            UiMsgFullRefillMeter -> return $ ui
                { _missingMeterFlash = Nothing
                , _unitOverlaySprite = Just . _unitRefill $ _sprites (ui :: MeterUI)
                }

            _ -> return ui

            where playerMeterVal = _value $ _meter player

updateMeterUI :: MsgsReadWrite UpdateWorldUiMsgsPhase m => Player -> MeterUI -> m MeterUI
updateMeterUI player meterUI = processMeterUiMsgs player meterUI'
    where
        unitOverlaySprite = case _unitOverlaySprite meterUI of
            Nothing                  -> Nothing
            Just spr
                | spriteFinished spr -> Nothing
                | otherwise          -> Just $ updateSprite spr

        imgs        = _images (meterUI :: MeterUI)
        backdropImg = case playerUpgradeCount MeterUpgradeType player of
            0 -> _backdropA imgs
            1 -> _backdropB imgs
            2 -> _backdropC imgs
            3 -> _backdropD imgs
            4 -> _backdropE imgs
            _ -> _backdropF imgs

        meterUI' = meterUI
            { _missingMeterFlash = updateMissingMeterFlash =<< _missingMeterFlash meterUI
            , _unitOverlaySprite = unitOverlaySprite
            , _backdropImage     = backdropImg
            }

drawMeterUI :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => Player -> MeterUI -> m ()
drawMeterUI player meterUI = do
    cfg <- readConfig _settings _ui

    let
        scale  = _overlayScale cfg
        scaleF = drawScaleToFloat scale

        meterOffset              = _meterOffset cfg `vecMul` scaleF
        meterPos                 = _overlayPos cfg `vecAdd` meterOffset
        MeterValue meterVal      = playerMeterValue $ _meter player
        meterUnitRelativeOffsets = _meterUnitRelativeOffsets cfg

        meterBackdropImg   = _backdropImage meterUI
        meterUnitFilledImg = _unitFilledImage meterUI

    drawImageScaled meterPos RightDir uiBackZIndex scale meterBackdropImg

    for_ (take meterVal meterUnitRelativeOffsets) $ \offset ->
        let pos = meterPos `vecAdd` (offset `vecMul` scaleF)
        in drawImageScaled pos RightDir uiBackZIndex scale meterUnitFilledImg

    sequenceA_ $ do
        mmf                 <- _missingMeterFlash meterUI
        meterUnitMissingImg <- spriteImage $ _sprite (mmf :: MissingMeterFlash)
        let offsets          = take (_unitCount mmf) (drop meterVal meterUnitRelativeOffsets)

        Just . for_ offsets $ \offset ->
            let pos = meterPos `vecAdd` (offset `vecMul` scaleF)
            in drawImageScaled pos RightDir uiBackZIndex scale meterUnitMissingImg

    case _unitOverlaySprite meterUI of
        Nothing             -> return ()
        Just unitOverlaySpr -> for_ (take meterVal meterUnitRelativeOffsets) $ \offset ->
            let pos = meterPos `vecAdd` (offset `vecMul` scaleF)
            in drawSpriteEx pos RightDir uiBackZIndex 0 FullOpacity scale unitOverlaySpr
