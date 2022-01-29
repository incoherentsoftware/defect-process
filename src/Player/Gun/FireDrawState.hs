module Player.Gun.FireDrawState
    ( module Player.Gun.FireDrawState.Types
    , mkGunFireDrawState
    , updateGunFireDrawState
    , updateGunFireDrawStateEx
    , gunFireDrawStateActive
    , gunFireDrawStateCancelable
    , drawGunFireDrawState
    , drawGunFireDrawStateEx
    ) where

import Control.Applicative    ((<|>))
import Control.Monad          (guard)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (sequenceA_)
import Data.Functor           ((<&>))
import Data.Maybe             (fromMaybe, isJust, isNothing)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

import Configs
import Constants
import FileCache
import Player.AimBody
import Player.Flags
import Player.Gun.FireDrawAngle
import Player.Gun.FireDrawData
import Player.Gun.MuzzleFlash
import Player.Gun.FireDrawState.LegsState
import Player.Gun.FireDrawState.Types
import Player.Types
import Player.Util
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

idleTransitionFrameTagName = FrameTagName "idleTransition" :: FrameTagName

updateGunFireDrawSprites :: GunFireDrawSprites -> GunFireDrawSprites
updateGunFireDrawSprites fireDrawSprs = fireDrawSprs
    { _head        = updateSprite $ _head fireDrawSprs
    , _torso       = updateSprite $ _torso fireDrawSprs
    , _leadArm     = updateSprite $ _leadArm fireDrawSprs
    , _rearArm     = updateSprite $ _rearArm fireDrawSprs
    , _muzzleFlash = updateSprite <$> _muzzleFlash (fireDrawSprs :: GunFireDrawSprites)
    }

gunFireDrawSpritesFinished :: GunFireDrawSprites -> Bool
gunFireDrawSpritesFinished fireDrawSprs = and
    [ spriteFinished $ _head fireDrawSprs
    , spriteFinished $ _torso fireDrawSprs
    , spriteFinished $ _leadArm fireDrawSprs
    , spriteFinished $ _rearArm fireDrawSprs
    , maybe True spriteFinished muzzleFlash
    ]
    where muzzleFlash = _muzzleFlash (fireDrawSprs :: GunFireDrawSprites)

mkGunFireDrawState :: (FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> m GunFireDrawState
mkGunFireDrawState pos = do
    legsSprs <- mkLegsSprites

    return $ GunFireDrawState
        { _activeSecs         = 0.0
        , _pos                = pos
        , _dir                = RightDir
        , _aimAngle           = 0.0
        , _legsState          = mkLegsState legsSprs
        , _gunFireDrawSprites = Nothing
        , _gunFireDrawData    = Nothing
        , _legsSprites        = legsSprs
        }

updateSpriteForIdleTransition :: GunFireDrawState -> Sprite -> Sprite
updateSpriteForIdleTransition gunFireDrawState spr
    | isExisting && idleTransitionFrameTagName `isSpriteFrameTag` spr = advanceSprite (_frameIndex spr + 1) spr
    | otherwise                                                       = spr
    where isExisting = isJust $ _gunFireDrawSprites gunFireDrawState

updateMuzzleFlashSprite
    :: MonadIO m
    => Bool
    -> Maybe GunFireDrawData
    -> GunFireDrawState
    -> m (Maybe Sprite)
updateMuzzleFlashSprite firedGun gunFireDrawData gunFireDrawState = case gunFireDrawData of
    Just fireDrawData
        | firedGun ->
            let
                muzzleFlash     = _muzzleFlash (fireDrawData :: GunFireDrawData)
                muzzleFlashSprs = (_sprites :: MuzzleFlash -> NE.NonEmpty Sprite) <$> muzzleFlash
            in sequenceA (randomChoice <$> muzzleFlashSprs) <&> \case
                Nothing             -> Nothing
                Just muzzleFlashSpr -> Just $ updateSpriteForIdleTransition gunFireDrawState muzzleFlashSpr

    _ ->
        let
            muzzleFlash = (_muzzleFlash :: GunFireDrawSprites -> Maybe Sprite) =<<
                _gunFireDrawSprites gunFireDrawState
        in case muzzleFlash of
            Nothing             -> return Nothing
            Just muzzleFlashSpr -> case updateSprite muzzleFlashSpr of
                muzzleFlashSpr'
                    | spriteFinished muzzleFlashSpr' -> return Nothing
                    | otherwise                      -> return $ Just muzzleFlashSpr'

updateGunFireDrawState
    :: (InputRead m, MonadIO m)
    => Bool
    -> Player
    -> Maybe GunFireDrawData
    -> GunFireDrawState
    -> m GunFireDrawState
updateGunFireDrawState cancelShoot player gunFireDrawData gunFireDrawState = do
    shootHeld <- aliasHold ShootAlias <$> readInputState
    let
        aimAngle
            | shootHeld = playerAimAngle player
            | otherwise = _aimAngle (gunFireDrawState :: GunFireDrawState)

        firedGun = _firedGun $ _flags (player :: Player)
        firedDir = if
            | gunFireDrawStateActive gunFireDrawState -> _dir (gunFireDrawState :: GunFireDrawState)
            | otherwise                               -> _dir (player :: Player)

    updateGunFireDrawStateEx cancelShoot player gunFireDrawData aimAngle firedGun firedDir gunFireDrawState

updateGunFireDrawStateEx
    :: MonadIO m
    => Bool
    -> Player
    -> Maybe GunFireDrawData
    -> Radians
    -> Bool
    -> Direction
    -> GunFireDrawState
    -> m GunFireDrawState
updateGunFireDrawStateEx cancelShoot player gunFireDrawData aimAngle firedGun firedDir gunFireDrawState =
    let
        updateSpriteForIdleTransition' = \spr -> updateSpriteForIdleTransition gunFireDrawState spr
        playerDir                      = _dir (gunFireDrawState :: GunFireDrawState)

        (gunFireDrawSprs, gunFireDrawData', legsSprites, aimAngle', playerDir') =
            fromMaybe (Nothing, Nothing, Nothing, aimAngle, playerDir) $ do
                guard $ not cancelShoot

                if
                    | firedGun -> do
                        let fireDrawAngle = calculateGunFireDrawAngle firedDir aimAngle
                        fireDrawData     <- gunFireDrawData
                        headSpr          <- fireDrawAngle `M.lookup` _headSprites fireDrawData
                        torsoSpr         <- fireDrawAngle `M.lookup` _torsoSprites fireDrawData
                        leadArmSpr       <- fireDrawAngle `M.lookup` _leadArmSprites fireDrawData
                        rearArmSpr       <- fireDrawAngle `M.lookup` _rearArmSprites fireDrawData

                        let
                            legsSprs =
                                _legsSprites (fireDrawData :: GunFireDrawData) <|>
                                Just (_legsSprites (gunFireDrawState :: GunFireDrawState))

                        return
                            ( Just $ GunFireDrawSprites
                                { _head        = updateSpriteForIdleTransition' headSpr
                                , _torso       = updateSpriteForIdleTransition' torsoSpr
                                , _leadArm     = updateSpriteForIdleTransition' leadArmSpr
                                , _rearArm     = updateSpriteForIdleTransition' rearArmSpr
                                , _muzzleFlash = Nothing
                                }
                            , Just $ fireDrawData {_fireDrawAngle = fireDrawAngle}
                            , legsSprs
                            , aimAngle
                            , firedDir
                            )

                    | otherwise -> do
                        fireDrawSprs <- _gunFireDrawSprites gunFireDrawState
                        guard $ not (gunFireDrawSpritesFinished fireDrawSprs)

                        let
                            fireDrawData       = _gunFireDrawData gunFireDrawState
                            restrictedAimAngle = case _fireDrawAngle <$> fireDrawData of
                                Nothing            -> aimAngle
                                Just fireDrawAngle -> restrictAngleByGunFireDrawAngle aimAngle playerDir fireDrawAngle

                        Just
                            ( Just $ updateGunFireDrawSprites fireDrawSprs
                            , fireDrawData
                            , Nothing
                            , restrictedAimAngle
                            , playerDir
                            )

        gettingHit                                    = _gettingHit $ _flags (player :: Player)
        activeSecs
            | isNothing gunFireDrawSprs || gettingHit = 0.0
            | otherwise                               = _activeSecs gunFireDrawState + timeStep

        legsState  = _legsState gunFireDrawState
        legsState' = case legsSprites of
            Nothing   -> updateLegsState player playerDir' legsState
            Just sprs -> resetLegsState sprs player playerDir' legsState
    in do
        gunFireDrawSprs' <- case gunFireDrawSprs of
            Nothing           -> return gunFireDrawSprs
            Just fireDrawSprs -> do
                muzzleFlashSpr <- updateMuzzleFlashSprite firedGun gunFireDrawData' gunFireDrawState
                return . Just $ (fireDrawSprs :: GunFireDrawSprites) {_muzzleFlash = muzzleFlashSpr}

        return $ gunFireDrawState
            { _activeSecs         = activeSecs
            , _pos                = _pos (player :: Player)
            , _dir                = playerDir'
            , _aimAngle           = aimAngle'
            , _legsState          = legsState'
            , _gunFireDrawSprites = gunFireDrawSprs'
            , _gunFireDrawData    = gunFireDrawData'
            }

gunFireDrawStateActive :: GunFireDrawState -> Bool
gunFireDrawStateActive = (> 0.0) . _activeSecs

gunFireDrawStateCancelable :: GunFireDrawState -> Bool
gunFireDrawStateCancelable gunFireDrawState
    | gunFireDrawStateActive gunFireDrawState =
        let
            uncancelableSecs =
                maybe defaultGunFireDrawStateUncancelableSecs _uncancelableSecs (_gunFireDrawData gunFireDrawState)
        in _activeSecs gunFireDrawState > uncancelableSecs
    | otherwise                               = True

drawGunFireDrawState :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => Player -> GunFireDrawState -> m ()
drawGunFireDrawState player gunFireDrawState = drawGunFireDrawStateEx player FullOpacity gunFireDrawState

drawGunFireDrawStateEx
    :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m)
    => Player
    -> Opacity
    -> GunFireDrawState
    -> m ()
drawGunFireDrawStateEx player opacity gunFireDrawState = do
    pos <- (_pos (gunFireDrawState :: GunFireDrawState) `vecAdd`) <$> playerLerpOffset player

    sequenceA_ $ do
        fireDrawSprs <- _gunFireDrawSprites gunFireDrawState
        fireDrawData <- _gunFireDrawData gunFireDrawState

        let
            aimAngle      = _aimAngle (gunFireDrawState :: GunFireDrawState)
            aimDir        = calculateAimAngleDir aimAngle
            fireDrawAngle = _fireDrawAngle fireDrawData

            playerDir
                | isForwardsGunFireDrawAngle fireDrawAngle = aimDir
                | otherwise                                = flipDirection aimDir

            legsState     = _legsState gunFireDrawState
            playerAimBody = (_calculatePlayerAimBody fireDrawData) fireDrawAngle pos aimAngle playerDir legsState

            leadShoulderPos = _leadShoulderPos playerAimBody
            rearShoulderPos = _rearShoulderPos playerAimBody
            hipsPos         = _hipsPos playerAimBody
            torsoAngle      = _torsoAngle playerAimBody
            neckPos         = _neckPos playerAimBody
            headAngle       = _headAngle playerAimBody

            drawLeadArm =
                let
                    leadArmAngle = _leadArmAngle playerAimBody
                    leadArmSpr   = _leadArm (fireDrawSprs :: GunFireDrawSprites)
                in drawSpriteEx leadShoulderPos playerDir playerBodyZIndex leadArmAngle opacity NonScaled leadArmSpr

            drawRearArm =
                let
                    rearArmAngle = _rearArmAngle playerAimBody
                    rearArmSpr   = _rearArm (fireDrawSprs :: GunFireDrawSprites)
                in drawSpriteEx rearShoulderPos playerDir playerBodyZIndex rearArmAngle opacity NonScaled rearArmSpr

            drawLegsTorso =
                let
                    legsSpr  = _sprite (legsState :: LegsState)
                    torsoSpr = _torso (fireDrawSprs :: GunFireDrawSprites)
                in do
                    drawSprite pos playerDir playerSpecialLegsZIndex legsSpr
                    drawSpriteEx hipsPos playerDir playerBodyZIndex torsoAngle opacity NonScaled torsoSpr

            drawHead =
                let headSpr = _head (fireDrawSprs :: GunFireDrawSprites)
                in drawSpriteEx neckPos playerDir playerBodyZIndex headAngle opacity NonScaled headSpr

        Just $ do
            case fireDrawAngle `M.lookup` _armOrders fireDrawData of
                Just DrawLeadArmInFront     -> drawRearArm >> drawLegsTorso >> drawHead >> drawLeadArm
                Just DrawRearArmInFront     -> drawLeadArm >> drawLegsTorso >> drawHead >> drawRearArm
                Just DrawRearArmHeadInFront -> drawLeadArm >> drawLegsTorso >> drawRearArm >> drawHead
                Just DrawBothArmsInFront    -> drawLegsTorso >> drawHead >> drawRearArm >> drawLeadArm
                Nothing                     -> return ()

            drawMuzzleFlash aimAngle aimDir fireDrawSprs fireDrawData playerAimBody opacity

drawMuzzleFlash
    :: (GraphicsReadWrite m, MonadIO m)
    => Radians
    -> Direction
    -> GunFireDrawSprites
    -> GunFireDrawData
    -> PlayerAimBody
    -> Opacity
    -> m ()
drawMuzzleFlash aimAngle aimDir gunFireDrawSprites gunFireDrawData playerAimBody opacity = sequenceA_ $ do
    muzzleFlash    <- _muzzleFlash (gunFireDrawData :: GunFireDrawData)
    muzzleFlashSpr <- _muzzleFlash (gunFireDrawSprites :: GunFireDrawSprites)

    let
        armShoulderPos = case _type (muzzleFlash :: MuzzleFlash) of
            LeadArmMuzzleFlash -> _leadShoulderPos playerAimBody
            RearArmMuzzleFlash -> _rearShoulderPos playerAimBody
        flashAngle     = calculateAimOverlayAngle aimAngle
        flashOffset    = vecFlipRotate (_offset muzzleFlash) aimDir flashAngle
        flashPos       = armShoulderPos `vecAdd` flashOffset

    Just $ drawSpriteEx flashPos aimDir playerGunMuzzleFlashZIndex flashAngle opacity NonScaled muzzleFlashSpr
