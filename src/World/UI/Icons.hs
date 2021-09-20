module World.UI.Icons
    ( IconsUI
    , mkIconsUI
    , updateIconsUI
    , drawIconsUI
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execState, modify)
import Data.Foldable          (for_)
import Data.Maybe             (fromMaybe, isJust)
import qualified Data.List as L
import qualified Data.Vector as V

import Configs
import Configs.All.Settings
import Configs.All.Settings.UI
import Constants
import FileCache
import Msg
import Player
import Player.Gun
import Player.MovementSkill
import Player.SecondarySkill
import Player.TimersCounters
import Player.Upgrade
import Player.Weapon
import Util
import Window.Graphics
import Window.InputState
import World.UI.Util
import World.ZIndex

emptyIconImageFileName       = "empty-icon.image"                  :: FileName
doubleJumpIconImageFileName  = "double-jump-icon.image"            :: FileName
invalidActionOverlayFileName = "invalid-action-icon-overlay.image" :: FileName

invalidActionOverlayFadeMultiplier    = 2.0 :: Float
secondarySkillsMaxNumUsageBlips       = 1   :: Int
showDoubleJumpIconMaxNumUsesThreshold = 1   :: Int

data IconType
    = EmptyIcon
    | NonEmptyIcon

data IconImage = IconImage
    { _type  :: IconType
    , _image :: Image
    }

loadIconImage :: (FileCache m, GraphicsRead m, MonadIO m) => FileName -> m IconImage
loadIconImage fileName = IconImage iconType <$> loadUiPackImage fileName
    where
        iconType
            | fileName == emptyIconImageFileName = EmptyIcon
            | otherwise                          = NonEmptyIcon

iconImageOpacity :: UiConfig -> IconImage -> Opacity
iconImageOpacity cfg iconImg = case _type (iconImg :: IconImage) of
    NonEmptyIcon -> FullOpacity
    EmptyIcon    -> _inactiveIconOpacity cfg

data WeaponIcons = WeaponIcons
    { _sword     :: IconImage
    , _gauntlets :: IconImage
    , _scythe    :: IconImage
    , _staff     :: IconImage
    }

mkWeaponIcons :: (FileCache m, GraphicsRead m, MonadIO m) => m WeaponIcons
mkWeaponIcons =
    WeaponIcons <$>
    loadIconImage "sword-icon.image" <*>
    loadIconImage "gauntlets-icon.image" <*>
    loadIconImage "scythe-icon.image" <*>
    loadIconImage "staff-icon.image"

data GunIcons = GunIcons
    { _revolver        :: IconImage
    , _shotgun         :: IconImage
    , _grenadeLauncher :: IconImage
    , _shardGun        :: IconImage
    , _spikeGun        :: IconImage
    }

mkGunIcons :: (FileCache m, GraphicsRead m, MonadIO m) => m GunIcons
mkGunIcons =
    GunIcons <$>
    loadIconImage "revolver-icon.image" <*>
    loadIconImage "shotgun-icon.image" <*>
    loadIconImage "grenade-launcher-icon.image" <*>
    loadIconImage "shard-gun-icon.image" <*>
    loadIconImage "spike-gun-icon.image"

data MovementSkillIcons = MovementSkillIcons
    { _dash      :: IconImage
    , _teleport  :: IconImage
    , _grapple   :: IconImage
    }

mkMovementSkillIcons :: (FileCache m, GraphicsRead m, MonadIO m) => m MovementSkillIcons
mkMovementSkillIcons =
    MovementSkillIcons <$>
    loadIconImage "dash-icon.image" <*>
    loadIconImage "teleport-icon.image" <*>
    loadIconImage "grapple-icon.image"

data SecondarySkillIcons = SecondarySkillIcons
    { _stoneForm :: IconImage
    , _flight    :: IconImage
    , _fastFall  :: IconImage
    }

mkSecondarySkillIcons :: (FileCache m, GraphicsRead m, MonadIO m) => m SecondarySkillIcons
mkSecondarySkillIcons =
    SecondarySkillIcons <$>
    loadIconImage "stone-form-icon.image" <*>
    loadIconImage "flight-icon.image" <*>
    loadIconImage "fast-fall-icon.image"

data InvalidActionOverlay = InvalidActionOverlay
    { _invalidActionOverlayImage    :: Image
    , _weaponActiveOpacity          :: Opacity
    , _weaponInactiveOpacity        :: Opacity
    , _gunActiveOpacity             :: Opacity
    , _gunInactiveOpacity           :: Opacity
    , _movementSkillOpacity         :: Opacity
    , _secondarySkillNeutralOpacity :: Opacity
    , _secondarySkillUpOpacity      :: Opacity
    , _secondarySkillDownOpacity    :: Opacity
    }

mkInvalidActionOverlay :: (FileCache m, GraphicsRead m, MonadIO m) => m InvalidActionOverlay
mkInvalidActionOverlay = do
    overlayImg <- loadUiPackImage invalidActionOverlayFileName
    return $ InvalidActionOverlay
        { _invalidActionOverlayImage    = overlayImg
        , _weaponActiveOpacity          = Opacity 0.0
        , _weaponInactiveOpacity        = Opacity 0.0
        , _gunActiveOpacity             = Opacity 0.0
        , _gunInactiveOpacity           = Opacity 0.0
        , _movementSkillOpacity         = Opacity 0.0
        , _secondarySkillNeutralOpacity = Opacity 0.0
        , _secondarySkillUpOpacity      = Opacity 0.0
        , _secondarySkillDownOpacity    = Opacity 0.0
        }

data IconsUI = IconsUI
    { _emptyIconImage                     :: IconImage
    , _doubleJumpIconImage                :: IconImage
    , _weaponIcons                        :: WeaponIcons
    , _gunIcons                           :: GunIcons
    , _movementSkillIcons                 :: MovementSkillIcons
    , _secondarySkillIcons                :: SecondarySkillIcons
    , _secondarySkillUpIconImageOverlay   :: IconImage
    , _secondarySkillDownIconImageOverlay :: IconImage
    , _invalidActionOverlay               :: InvalidActionOverlay
    , _usageBlipImage                     :: Image
    , _usageBlipBackdropImage             :: Image
    }

mkIconsUI :: (FileCache m, GraphicsRead m, MonadIO m) => m IconsUI
mkIconsUI =
    IconsUI <$>
    loadIconImage emptyIconImageFileName <*>
    loadIconImage doubleJumpIconImageFileName <*>
    mkWeaponIcons <*>
    mkGunIcons <*>
    mkMovementSkillIcons <*>
    mkSecondarySkillIcons <*>
    loadIconImage "secondary-skill-up-icon-overlay.image" <*>
    loadIconImage "secondary-skill-down-icon-overlay.image" <*>
    mkInvalidActionOverlay <*>
    loadUiPackImage "usage-blip.image" <*>
    loadUiPackImage "usage-blip-backdrop.image"

iconsUiWeaponIconImages :: Player -> IconsUI -> (IconImage, IconImage)
iconsUiWeaponIconImages player iconsUI = (activeImg, inactiveImg)
    where
        toIconImage :: [WeaponType] -> IconImage
        toIconImage = \case
            []                  -> _emptyIconImage iconsUI
            (SwordWeapon:_)     -> _sword weaponIcons
            (GauntletsWeapon:_) -> _gauntlets weaponIcons
            (ScytheWeapon:_)    -> _scythe weaponIcons
            (StaffWeapon:_)     -> _staff weaponIcons
            where weaponIcons = _weaponIcons iconsUI

        weaponTypes = playerWeaponTypes player
        activeImg   = toIconImage weaponTypes
        inactiveImg = toIconImage $ safeTail weaponTypes

iconsUiGunIconImages :: Player -> IconsUI -> (IconImage, IconImage)
iconsUiGunIconImages player iconsUI = (activeImg, inactiveImg)
    where
        toIconImage :: [GunType] -> IconImage
        toIconImage = \case
            []                     -> _emptyIconImage iconsUI
            (RevolverGun:_)        -> _revolver gunIcons
            (ShotgunGun:_)         -> _shotgun gunIcons
            (GrenadeLauncherGun:_) -> _grenadeLauncher gunIcons
            (SpikeGun:_)           -> _spikeGun gunIcons
            (ShardGun:_)           -> _shardGun gunIcons
            where gunIcons = _gunIcons iconsUI

        gunTypes    = playerGunTypes player
        activeImg   = toIconImage gunTypes
        inactiveImg = toIconImage $ safeTail gunTypes

iconsUiMoveSkillIconImage :: Player -> IconsUI -> IconImage
iconsUiMoveSkillIconImage player iconsUI = case playerMovementSkillType player of
    Nothing            -> _emptyIconImage iconsUI
    Just DashSkill     -> _dash moveSkillIcons
    Just TeleportSkill -> _teleport moveSkillIcons
    Just GrappleSkill  -> _grapple moveSkillIcons
    where moveSkillIcons = _movementSkillIcons iconsUI

iconsUiSecondarySkillIconImages :: Player -> IconsUI -> (IconImage, IconImage, IconImage)
iconsUiSecondarySkillIconImages player iconsUI =
    ( toIconImage $ playerSecondarySkillType SecondarySkillNeutralSlot player
    , toIconImage $ playerSecondarySkillType SecondarySkillUpSlot player
    , toIconImage $ playerSecondarySkillType SecondarySkillDownSlot player
    )
    where
        toIconImage :: Maybe SecondarySkillType -> IconImage
        toIconImage = \case
            Nothing             -> _emptyIconImage iconsUI
            Just StoneFormSkill -> _stoneForm secondarySkillIcons
            Just FlightSkill    -> _flight secondarySkillIcons
            Just FastFallSkill  -> _fastFall secondarySkillIcons
            where secondarySkillIcons = _secondarySkillIcons iconsUI

updateIconsUI :: MsgsRead UpdateWorldUiMsgsPhase m => IconsUI -> m IconsUI
updateIconsUI iconsUI =
    let
        updateInvalidActionOverlay :: IconsUI -> IconsUI
        updateInvalidActionOverlay ui = ui {_invalidActionOverlay = invalidActionOverlay'}
            where
                invalidActionOverlay = _invalidActionOverlay ui
                decreaseOpacity'     = \f ->
                    decreaseOpacity (invalidActionOverlayFadeMultiplier * timeStep) (f invalidActionOverlay)

                invalidActionOverlay' = invalidActionOverlay
                    { _weaponActiveOpacity          = decreaseOpacity' _weaponActiveOpacity
                    , _weaponInactiveOpacity        = decreaseOpacity' _weaponInactiveOpacity
                    , _gunActiveOpacity             = decreaseOpacity' _gunActiveOpacity
                    , _gunInactiveOpacity           = decreaseOpacity' _gunInactiveOpacity
                    , _movementSkillOpacity         = decreaseOpacity' _movementSkillOpacity
                    , _secondarySkillNeutralOpacity = decreaseOpacity' _secondarySkillNeutralOpacity
                    , _secondarySkillUpOpacity      = decreaseOpacity' _secondarySkillUpOpacity
                    , _secondarySkillDownOpacity    = decreaseOpacity' _secondarySkillDownOpacity
                    }

        processMsg :: IconsUI -> UiMsgPayload -> IconsUI
        processMsg ui p = case p of
            UiMsgInvalidAction inputAlias ->
                let
                    invalidActionOverlay' = case inputAlias of
                        WeaponAlias         -> invalidActionOverlay {_weaponActiveOpacity = Opacity 1.0}
                        SwitchWeaponAlias   -> invalidActionOverlay {_weaponInactiveOpacity = Opacity 1.0}
                        ShootAlias          -> invalidActionOverlay {_gunActiveOpacity = Opacity 1.0}
                        SwitchGunAlias      -> invalidActionOverlay {_gunInactiveOpacity = Opacity 1.0}
                        MovementSkillAlias  -> invalidActionOverlay {_movementSkillOpacity = Opacity 1.0}
                        SecondarySkillAlias -> invalidActionOverlay {_secondarySkillNeutralOpacity = Opacity 1.0}
                        _                   -> invalidActionOverlay
                in ui {_invalidActionOverlay = invalidActionOverlay'}

            UiMsgInvalidActionEx inputAlias1 inputAlias2 ->
                let
                    invalidActionOverlay' = case (inputAlias1, inputAlias2) of
                        (SecondarySkillAlias, UpAlias)   ->
                            invalidActionOverlay {_secondarySkillUpOpacity = Opacity 1.0}
                        (SecondarySkillAlias, DownAlias) ->
                            invalidActionOverlay {_secondarySkillDownOpacity = Opacity 1.0}
                        _                                -> invalidActionOverlay
                in ui {_invalidActionOverlay = invalidActionOverlay'}

            _ -> ui

            where invalidActionOverlay = _invalidActionOverlay ui
    in do
        msgs <- readMsgs
        return . flip execState iconsUI $ do
            modify updateInvalidActionOverlay
            modify $ \ui -> L.foldl' processMsg ui msgs

drawIconsUI :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => Player -> IconsUI -> m ()
drawIconsUI player iconsUI = do
    cfg <- readConfig _settings _ui

    let
        scale                   = _overlayScale cfg
        scaleF                  = drawScaleToFloat scale
        invalidActionOverlay    = _invalidActionOverlay iconsUI
        invalidActionOverlayImg = _invalidActionOverlayImage invalidActionOverlay

        drawImage' :: (GraphicsReadWrite m1, MonadIO m1) => Pos2 -> Opacity -> DrawScale -> Image -> m1 ()
        drawImage' pos opacity drwScale img = drawImageEx pos RightDir uiFrontZIndex 0.0 opacity drwScale img

        drawActiveIcon
            :: (GraphicsReadWrite m1, MonadIO m1)
            => Pos2
            -> IconImage
            -> (InvalidActionOverlay -> Opacity)
            -> m1 ()
        drawActiveIcon pos iconImg invalidOverlayOpacityF = do
            drawImage' pos (iconImageOpacity cfg iconImg) scale (_image iconImg)
            drawImage' pos (invalidOverlayOpacityF invalidActionOverlay) scale invalidActionOverlayImg

        drawInactiveIcon
            :: (GraphicsReadWrite m1, MonadIO m1)
            => Pos2
            -> IconImage
            -> (InvalidActionOverlay -> Opacity)
            -> m1 ()
        drawInactiveIcon pos iconImg invalidOverlayOpacityF = do
            let scale' = Scaled $ drawScaleToFloat scale * _inactiveIconRelativeScale cfg
            drawImage' pos (_inactiveIconOpacity cfg) scale' (_image iconImg)
            drawImage' pos (invalidOverlayOpacityF invalidActionOverlay) scale' invalidActionOverlayImg

        drawIconOverlay :: (GraphicsReadWrite m1, MonadIO m1) => Pos2 -> IconImage -> IconImage -> m1 ()
        drawIconOverlay pos iconImg iconOverlayImg =
            drawImage' pos (iconImageOpacity cfg iconImg) scale (_image iconOverlayImg)

        drawUsageBlips :: (GraphicsReadWrite m1, MonadIO m1) => Pos2 -> Int -> Int -> m1 ()
        drawUsageBlips pos numUses maxNumUses =
            let
                usageBlipRelativeOffsets = _usageBlipRelativeOffsets cfg
                idx                      = min (maxNumUses - 1) (V.length usageBlipRelativeOffsets - 1)
                offsets                  = fromMaybe [] (usageBlipRelativeOffsets V.!? idx)
            in do
                for_ offsets $ \offset ->
                    drawImage' (pos `vecAdd` offset) FullOpacity scale (_usageBlipBackdropImage iconsUI)

                for_ (zip [0..numUses - 1] offsets) $ \(_, offset) ->
                    drawImage' (pos `vecAdd` offset) FullOpacity scale (_usageBlipImage iconsUI)

        drawSecondarySkillUsageBlip :: (GraphicsReadWrite m1, MonadIO m1) => Pos2 -> SecondarySkillSlot -> m1 ()
        drawSecondarySkillUsageBlip pos slot = when (isJust (playerSecondarySkillType slot player)) $
            let numUses = if playerSecondarySkillOnCooldown slot player then 0 else 1
            in drawUsageBlips pos numUses secondarySkillsMaxNumUsageBlips

    let
        overlayPos                                   = _overlayPos cfg
        weaponActiveIconOffset                       = _weaponActiveIconOffset cfg `vecMul` scaleF
        weaponActiveIconPos                          = overlayPos `vecAdd` weaponActiveIconOffset
        weaponInactiveIconOffset                     = _weaponInactiveIconOffset cfg `vecMul` scaleF
        weaponInactiveIconPos                        = overlayPos `vecAdd` weaponInactiveIconOffset
        (weaponActiveIconImg, weaponInactiveIconImg) = iconsUiWeaponIconImages player iconsUI
    drawActiveIcon weaponActiveIconPos weaponActiveIconImg _weaponActiveOpacity
    drawInactiveIcon weaponInactiveIconPos weaponInactiveIconImg _weaponInactiveOpacity

    let
        (gunActiveIconImg, gunInactiveIconImg) = iconsUiGunIconImages player iconsUI
        gunActiveIconOffset                    = _gunActiveIconOffset cfg `vecMul` scaleF
        gunActiveIconPos                       = overlayPos `vecAdd` gunActiveIconOffset
        gunInactiveIconOffset                  = _gunInactiveIconOffset cfg `vecMul` scaleF
        gunInactiveIconPos                     = overlayPos `vecAdd` gunInactiveIconOffset
    drawActiveIcon gunActiveIconPos gunActiveIconImg _gunActiveOpacity
    drawInactiveIcon gunInactiveIconPos gunInactiveIconImg _gunInactiveOpacity

    let
        moveSkillIconImg    = iconsUiMoveSkillIconImage player iconsUI
        moveSkillIconOffset = _movementSkillIconOffset cfg `vecMul` scaleF
        moveSkillIconPos    = overlayPos `vecAdd` moveSkillIconOffset
    drawActiveIcon moveSkillIconPos moveSkillIconImg _movementSkillOpacity
    drawUsageBlips moveSkillIconPos (playerMovementSkillNumCharges player) (playerMovementSkillMaxNumCharges player)

    let
        secondarySkillNeutralIconOffset = _secondarySkillNeutralIconOffset cfg `vecMul` scaleF
        secondarySkillUpIconOffset      = _secondarySkillUpIconOffset cfg `vecMul` scaleF
        secondarySkillDownIconOffset    = _secondarySkillDownIconOffset cfg `vecMul` scaleF
        secondarySkillNeutralIconPos    = overlayPos `vecAdd` secondarySkillNeutralIconOffset
        secondarySkillUpIconPos         = overlayPos `vecAdd` secondarySkillUpIconOffset
        secondarySkillDownIconPos       = overlayPos `vecAdd` secondarySkillDownIconOffset

        (secondarySkillNeutralIconImg, secondarySkillUpIconImg, secondarySkillDownIconImg) =
            iconsUiSecondarySkillIconImages player iconsUI

    drawActiveIcon secondarySkillNeutralIconPos secondarySkillNeutralIconImg _secondarySkillNeutralOpacity
    drawSecondarySkillUsageBlip secondarySkillNeutralIconPos SecondarySkillNeutralSlot

    drawActiveIcon secondarySkillUpIconPos secondarySkillUpIconImg _secondarySkillUpOpacity
    drawIconOverlay secondarySkillUpIconPos secondarySkillUpIconImg  (_secondarySkillUpIconImageOverlay iconsUI)
    drawSecondarySkillUsageBlip secondarySkillUpIconPos SecondarySkillUpSlot

    drawActiveIcon secondarySkillDownIconPos secondarySkillDownIconImg _secondarySkillDownOpacity
    drawIconOverlay secondarySkillDownIconPos secondarySkillDownIconImg  (_secondarySkillDownIconImageOverlay iconsUI)
    drawSecondarySkillUsageBlip secondarySkillDownIconPos SecondarySkillDownSlot

    let doubleJumpMaxNumUses = playerUpgradeCount DoubleJumpUpgradeType player + 1
    when (doubleJumpMaxNumUses > showDoubleJumpIconMaxNumUsesThreshold) $
        let
            pos               = overlayPos `vecAdd` _doubleJumpIconOffset cfg
            doubleJumpNumUses = doubleJumpMaxNumUses - _doubleJumpCounter (_timersCounters player)
        in do
            drawActiveIcon pos (_doubleJumpIconImage iconsUI) (const $ Opacity 0.0)
            drawUsageBlips pos doubleJumpNumUses doubleJumpMaxNumUses
