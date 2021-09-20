module World.UI.Health
    ( HealthUI
    , mkHealthUI
    , updateHealthUI
    , drawHealthUI
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack.Util
import Configs
import Configs.All.Settings
import Configs.All.Settings.UI
import Constants
import FileCache
import Player
import Util
import Window.Graphics
import World.UI.Util
import World.ZIndex

maxPlayerHealthsCount = 2 :: Int

data HealthUI = HealthUI
    { _healths                    :: [Health]
    , _healthDelta                :: Int
    , _sprite                     :: Maybe Sprite
    , _spriteOpacity              :: Opacity
    , _healthbarInnerDamageSprite :: Sprite
    , _healthbarBackdropImage     :: Image
    , _healthbarInnerImage        :: Image
    }

mkHealthUI :: (FileCache m, GraphicsRead m, MonadIO m) => m HealthUI
mkHealthUI = do
    healthbarBackdropImg    <- loadUiPackImage "healthbar-backdrop.image"
    healthbarInnerImg       <- loadUiPackImage "healthbar-inner.image"
    healthbarInnerDamageSpr <- loadUiPackSprite "healthbar-inner-damage.spr"

    return $ HealthUI
        { _healths                    = []
        , _healthDelta                = 0
        , _sprite                     = Nothing
        , _spriteOpacity              = Opacity 0.0
        , _healthbarInnerDamageSprite = healthbarInnerDamageSpr
        , _healthbarBackdropImage     = healthbarBackdropImg
        , _healthbarInnerImage        = healthbarInnerImg
        }

updateHealthUI :: ConfigsRead m => Player -> HealthUI -> m HealthUI
updateHealthUI player healthUI = do
    cfg <- readConfig _settings _ui

    let
        hp         = _health player
        healths    = take maxPlayerHealthsCount (hp:_healths healthUI)
        sprOpacity = _spriteOpacity healthUI

        hpDelta  = _healthDelta healthUI
        hpDelta' = case maybeLast healths of
            Just prevHp
                | _value prevHp > _value hp         -> hpDelta + (_value prevHp - _value hp)
                | Opacity o <- sprOpacity, o <= 0.0 -> 0
            _                                       -> _healthDelta healthUI

        spr
            | hpDelta' > hpDelta = Just $ _healthbarInnerDamageSprite healthUI
            | otherwise          = updateSprite <$> _sprite (healthUI :: HealthUI)

        sprFinished = maybe False spriteFinished spr
        sprOpacity' = case sprOpacity of
            _
                | hpDelta' > hpDelta -> Opacity 1.0
            Opacity o
                | sprFinished        ->
                    let healthbarDamageFadeMultiplier = _healthbarDamageFadeMultiplier cfg
                    in Opacity $ max 0.0 (o - timeStep * healthbarDamageFadeMultiplier)
            opacity                  -> opacity

    return $ healthUI
        { _healths       = healths
        , _healthDelta   = hpDelta'
        , _sprite        = spr
        , _spriteOpacity = sprOpacity'
        }

drawHealthUI :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => Player -> HealthUI -> m ()
drawHealthUI player healthUI = do
    cfg <- readConfig _settings _ui
    let
        scale  = _overlayScale cfg
        scaleF = drawScaleToFloat scale

        hpBackdropImg = _healthbarBackdropImage healthUI
        hpInnerImg    = _healthbarInnerImage healthUI

        healthbarInnerOffset@(Pos2 healthbarInnerOffsetX _) = _healthbarInnerOffset cfg

        hp               = _health player
        hpVal            = fromIntegral $ _value hp
        maxHpVal         = fromIntegral $ _maxValue hp
        hpPercent        = hpVal / maxHpVal
        hpInnerAdjust    = (scaleF - 1.0) * 0.5
        hpInnerImgWidth  = imageWidth hpInnerImg * hpPercent * scaleF + healthbarInnerOffsetX * hpInnerAdjust
        hpInnerImgHeight = imageHeight hpInnerImg * scaleF

        overlayPos             = _overlayPos cfg
        healthbarInnerOffset'  =
            (healthbarInnerOffset `vecMul` scaleF) `vecSub` (healthbarInnerOffset `vecMul` hpInnerAdjust)
        healthbarInnerPos      = overlayPos `vecAdd` healthbarInnerOffset'
        healthbarInnerPosRight = vecX healthbarInnerPos + hpInnerImgWidth

    drawImageScaled overlayPos RightDir uiBackZIndex scale hpBackdropImg
    drawImageRect healthbarInnerPos hpInnerImgWidth hpInnerImgHeight uiFrontZIndex hpInnerImg

    case spriteImage (_healthbarInnerDamageSprite healthUI) of
        Nothing        -> return ()
        Just damageImg ->
            let
                hpDelta         = _healthDelta healthUI
                hpDeltaPercent  = fromIntegral hpDelta / maxHpVal
                damageImgWidth  = imageWidth damageImg * hpDeltaPercent * scaleF
                damageImgHeight = imageHeight damageImg * scaleF

                overlayY = vecY $ _overlayPos cfg
                pos      = Pos2 healthbarInnerPosRight overlayY
                opacity  = _spriteOpacity healthUI
            in drawImageRectEx pos damageImgWidth damageImgHeight uiFrontZIndex 0.0 opacity NonScaled damageImg
