module Player.Weapon.All.Gauntlets
    ( mkGauntletsWeapon
    ) where

import Control.Applicative    ((<|>))
import Control.Monad          (guard, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (sequenceA_)
import Data.Maybe             (fromMaybe, listToMaybe)
import System.FilePath        (takeBaseName)
import qualified Data.Map as M

import Attack
import Attack.Projectile
import Configs
import Configs.All.Player
import Configs.All.PlayerWeapon.Gauntlets
import FileCache
import Msg
import Player
import Player.BufferedInputState
import Player.Weapon
import Player.Weapon.All.Gauntlets.Data
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

releasableFrameTagName      = FrameTagName "releasable"      :: FrameTagName
projectileFrameTagName      = FrameTagName "projectile"      :: FrameTagName
groundedFrameTagName        = FrameTagName "grounded"        :: FrameTagName
doubleKickFrameTagName      = FrameTagName "doubleKick"      :: FrameTagName
kickFrameTagName            = FrameTagName "kick"            :: FrameTagName
airForwardsKickFrameTagName = FrameTagName "airForwardsKick" :: FrameTagName

chargeOverlaySoundPath = "event:/SFX Events/Player/Weapons/Gauntlets/charge-overlay-c" :: FilePath

mkGauntletsWeapon :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some Weapon)
mkGauntletsWeapon = do
    gauntletsData <- mkGauntletsData
    return . Some $ (mkWeapon gauntletsData GauntletsWeapon)
        { _think       = thinkGauntlets
        , _update      = updateGauntlets
        , _drawOverlay = drawGauntletsOverlay
        }

thinkGauntlets :: (InputRead m, MonadIO m, MsgsWrite ThinkPlayerMsgsPhase m) => WeaponThink GauntletsData m
thinkGauntlets weaponThinkStatus player currentAtk gauntlets = do
    atkFromInput <- case weaponThinkStatus of
        WeaponThinkForeground WeaponAttackReady -> mkAttackFromInput gauntlets player currentAtk
        _                                       -> return Nothing

    newAtk <- case atkFromInput of
        Nothing -> maybe (return Nothing) (thinkGauntletsAttack gauntlets player) currentAtk
        Just _  -> return atkFromInput

    case newAtk of
        Nothing  -> return []
        Just atk ->
            let
                updateReleaseHeld = \w -> w
                    { _data = (_data w)
                        { _releaseHeld               = True
                        , _attackHitEnemy            = False
                        , _attackSeenReleasableFrame = False
                        }
                    }
            in return
                [ mkMsg $ PlayerMsgSetAttack atk
                , mkMsg $ PlayerMsgUpdateWeapon updateReleaseHeld
                ]

updateGauntlets :: (InputRead m, MsgsWrite UpdatePlayerMsgsPhase m) => WeaponUpdate GauntletsData m
updateGauntlets WeaponUpdateBackground _ _ gauntlets        = return gauntlets
updateGauntlets WeaponUpdateForeground player atk gauntlets =
    let
        attackIs' :: AttackDescription -> Bool
        attackIs' atkDesc = maybe False (`attackIs` atkDesc) atk

        gauntletsData        = _data gauntlets
        isAtkReleasableFrame = maybe False (releasableFrameTagName `isAttackFrameTag`) atk
        dashPunchForwards    = _dashPunchForwards $ _attackDescriptions gauntletsData

        enemyInAttackRange
            | attackIs' dashPunchForwards = _enemyInAttackRange gauntletsData
            | otherwise                   = False

        updateData :: InputState -> GauntletsData
        updateData inputState = gauntletsData
            { _enemyInAttackRange        = enemyInAttackRange
            , _releaseHeld               = releaseHeld
            , _attackSeenReleasableFrame = atkSeenReleasableFrame
            , _overlaySprite             = overlaySpr
            , _chargeOverlaySprite       = updateSprite $ _chargeOverlaySprite gauntletsData
            }
            where
                releaseHeld
                    | WeaponAlias `aliasNotHold` inputState = False
                    | otherwise                             = _releaseHeld gauntletsData

                atkSeenReleasableFrame = _attackSeenReleasableFrame gauntletsData || isAtkReleasableFrame

                overlaySpr = case _overlaySprite gauntletsData of
                    Just spr
                        | not isAtkReleasableFrame -> Nothing
                        | otherwise                -> Just $ updateSprite spr

                    Nothing
                        | isAtkReleasableFrame && isReleasableAtkConditionsMet gauntletsData -> do
                            atkDesc <- _description <$> atk
                            atkDesc `M.lookup` _releasableAtkIndicatorSprs gauntletsData
                        | otherwise                                                          -> Nothing

    in do
        gauntletsData' <- updateData <$> readInputState

        when (isReleasableAtkConditionsMet gauntletsData') $
            let
                hashedId = _chargeSoundHashedId gauntletsData'
                pos      = _pos (player :: Player)
            in writeMsgs [mkMsg $ AudioMsgPlaySoundContinuous chargeOverlaySoundPath hashedId pos]

        return $ gauntlets {_data = gauntletsData'}

mkReleaseAttackFromInput :: (InputRead m, MonadIO m) => GauntletsData -> Player -> m Attack
mkReleaseAttackFromInput gauntletsData player = do
    inputState <- readInputState

    let
        aliasHold' :: InputAlias -> Bool
        aliasHold' alias = alias `aliasHold` inputState

        wpnAtkDescs           = _attackDescriptions gauntletsData
        groundPunchRelease    = _groundPunchRelease wpnAtkDescs
        flyingKickRelease     = _flyingKickRelease wpnAtkDescs
        palmExplodeRelease    = _palmExplodeRelease wpnAtkDescs
        diveKickRelease       = _diveKickRelease wpnAtkDescs
        fallingAxeKickRelease = _fallingAxeKickRelease wpnAtkDescs

        pos                         = _pos (player :: Player)
        dir
            | aliasHold' LeftAlias  = LeftDir
            | aliasHold' RightAlias = RightDir
            | otherwise             = _dir (player :: Player)
        onGround                    = _touchingGround $ _flags player

        atk
            | onGround  = if
                | aliasHold' UpAlias   -> flyingKickRelease
                | aliasHold' DownAlias -> groundPunchRelease
                | otherwise            -> palmExplodeRelease
            | otherwise = if
                | aliasHold' DownAlias -> fallingAxeKickRelease
                | otherwise            -> diveKickRelease

    mkAttack pos dir atk

mkAttackFromInput :: (InputRead m, MonadIO m) => Weapon GauntletsData -> Player -> Maybe Attack -> m (Maybe Attack)
mkAttackFromInput gauntlets player atk =
    let
        attackIs'   = \atkDesc -> maybe False (`attackIs` atkDesc) atk
        attackIn'   = \atkDescs -> maybe False (`attackIn` atkDescs) atk
        attackNotIn = \atkDescs -> not $ attackIn' atkDescs

        gauntletsData             = _data gauntlets
        pos                       = _pos (player :: Player)
        onGround                  = _touchingGround $ _flags player
        inAir                     = not onGround
        atkCancelable             = maybe True attackCancelable atk
        isDoubleKickFrameTag      = maybe False (doubleKickFrameTagName `isAttackFrameTag`) atk
        isKickFrameTag            = maybe False (kickFrameTagName `isAttackFrameTag`) atk
        isAirForwardsKickFrameTag = maybe False (airForwardsKickFrameTagName `isAttackFrameTag`) atk

        wpnAtkDescs       = _attackDescriptions gauntletsData
        punch1            = _punch1 wpnAtkDescs
        punch2            = _punch2 wpnAtkDescs
        doubleKick1       = _doubleKick1 wpnAtkDescs
        doubleKick2       = _doubleKick2 wpnAtkDescs
        punch3            = _punch3 wpnAtkDescs
        kick              = _kick wpnAtkDescs
        risingUppercut    = _risingUppercut wpnAtkDescs
        evasiveKick       = _evasiveKick wpnAtkDescs
        diveKick          = _diveKick wpnAtkDescs
        diveKickBounce    = _diveKickBounce wpnAtkDescs
        dashPunchForwards = _dashPunchForwards wpnAtkDescs
        dashPunch         = _dashPunch wpnAtkDescs
        airPunch1         = _airPunch1 wpnAtkDescs
        airForwardsKick1  = _airForwardsKick1 wpnAtkDescs
        airPunch2         = _airPunch2 wpnAtkDescs
        airPunch3         = _airPunch3 wpnAtkDescs
        diveKickRelease   = _diveKickRelease wpnAtkDescs
    in do
        inputState <- readInputState

        let
            leftHeld        = LeftAlias `aliasHold` inputState
            rightHeld       = RightAlias `aliasHold` inputState
            dir
                | leftHeld  = LeftDir
                | rightHeld = RightDir
                | otherwise = _dir (player :: Player)

            mkAttack'       = \atkDesc -> Just <$> mkAttack pos dir atkDesc
            mkAttackWithDir = \atkDesc atkDir -> Just <$> mkAttack pos atkDir atkDesc

            weaponPressed     = WeaponAlias `aliasPressed` inputState || WeaponInput `inPlayerInputBuffer` player
            weaponUpPressed   =
                (WeaponAlias `aliasPressed` inputState && UpAlias `aliasHold` inputState) ||
                WeaponUpInput `inPlayerInputBuffer` player
            weaponDownPressed =
                (WeaponAlias `aliasPressed` inputState && DownAlias `aliasHold` inputState) ||
                WeaponDownInput `inPlayerInputBuffer` player
            weaponNotHeld     = not $ WeaponAlias `aliasHold` inputState

        if
            -- dash punch (left)
            | (inPlayerTapInputBuffer [LeftInput, LeftInput] player || isPlayerInputBufferQCF LeftDir player) &&
              not rightHeld && weaponPressed && onGround && attackNotIn [dashPunchForwards, dashPunch] ->
                mkAttackWithDir dashPunchForwards LeftDir

            -- dash punch (right)
            | (inPlayerTapInputBuffer [RightInput, RightInput] player || isPlayerInputBufferQCF RightDir player) &&
              not leftHeld && weaponPressed && onGround && attackNotIn [dashPunchForwards, dashPunch] ->
                mkAttackWithDir dashPunchForwards RightDir

            -- dive kick
            | inAir && weaponDownPressed && attackNotIn [diveKick, diveKickBounce, diveKickRelease] ->
                mkAttack' diveKick

            -- rising uppercut
            | onGround && weaponUpPressed && not (attackIs' risingUppercut) -> mkAttack' risingUppercut

            -- evasive kick
            | onGround && weaponDownPressed && not (attackIs' evasiveKick) -> mkAttack' evasiveKick

            | weaponPressed && onGround -> if
                -- kick
                | attackIn' [punch2, doubleKick2] && isKickFrameTag -> mkAttack' kick
                -- punch3
                | attackIn' [punch2, doubleKick2] && atkCancelable  -> mkAttack' punch3
                -- double kick
                | attackIs' punch1 && isDoubleKickFrameTag          -> mkAttack' doubleKick1
                -- punch2
                | attackIs' punch1 && atkCancelable                 -> mkAttack' punch2
                -- punch1
                | otherwise                                         -> if
                    | attackNotIn [doubleKick1, punch3, kick] -> mkAttack' punch1
                    | otherwise                               -> return Nothing

            | weaponPressed && inAir -> if
                -- air forwards kick
                | attackIn' [airPunch2, airPunch1] && isAirForwardsKickFrameTag -> mkAttack' airForwardsKick1
                -- air punch3
                | attackIs' airPunch2 && atkCancelable                          -> mkAttack' airPunch3
                -- air punch2
                | attackIs' airPunch1 && atkCancelable                          -> mkAttack' airPunch2
                -- air punch1
                | otherwise                                                     -> if
                    | attackNotIn [airPunch3] -> mkAttack' airPunch1
                    | otherwise               -> return Nothing

            | weaponNotHeld && isReleasableAtkConditionsMet gauntletsData ->
                Just <$> mkReleaseAttackFromInput gauntletsData player

            | otherwise -> return Nothing

thinkGauntletsAttack
    :: (MonadIO m, MsgsWrite ThinkPlayerMsgsPhase m)
    => Weapon GauntletsData
    -> Player
    -> Attack
    -> m (Maybe Attack)
thinkGauntletsAttack gauntlets player atk
    -- dive kick/dive kick release -> dive kick land
    | attackIn' [_diveKick, _diveKickRelease] && onGround = mkAttack' _diveKickLand

    -- falling axe kick release -> falling axe kick land
    | attackIs' _fallingAxeKickRelease && onGround = mkAttack' _fallingAxeKickLand

    -- dash punch forwards -> dash punch
    | attackIs' _dashPunchForwards = if
        | inAir                             -> mkAttack' _fall
        | _enemyInAttackRange gauntletsData -> mkAttack' _dashPunch
        | otherwise                         -> return Nothing

    -- evasive kick
    | attackIs' _evasiveKick && attackFrameChanged atk && projectileFrameTagName `isAttackFrameTag` atk =
        let
            Pos2 offsetX offsetY = _evasiveKickProjectileOffset $ _config (gauntletsData :: GauntletsData)
            offset               = Pos2 (offsetX * directionNeg dir) offsetY
            pos'                 = pos `vecAdd` offset
            evasiveKickProj      = _evasiveKickProjectile wpnAtkDescs
        in do
            writeMsgs [mkMsg $ NewThinkProjectileMsgAddM (mkPlayerAttackProjectile pos' dir evasiveKickProj)]
            return Nothing

    -- air punch1/air punch2/air forwards kick/air punch3 land
    | attackIn' [_airPunch1, _airPunch2, _airForwardsKick1, _airForwardsKick2] && onGround = return finishAtk

    -- cancel grounded attacks if now in air
    | inAir && attackIn' groundedAtkDescs = return finishAtk

    -- cancel partial grounded attacks if now in air
    | inAir && attackIn' partialGroundedAtkDescs && groundedFrameTagName `isAttackFrameTag` atk = return finishAtk

    | otherwise = return Nothing

    where
        pos           = _pos (player :: Player)
        dir           = _dir (player :: Player)
        wpnAtkDescs   = _attackDescriptions gauntletsData
        gauntletsData = _data gauntlets
        onGround      = _touchingGround $ _flags player
        inAir         = not onGround

        mkAttack' = \atkDescF -> Just <$> mkAttack pos dir (atkDescF wpnAtkDescs)
        attackIs'   = \atkDescF -> atk `attackIs` (atkDescF wpnAtkDescs)
        attackIn'   = \atkDescFs -> atk `attackIn` [atkDescF wpnAtkDescs | atkDescF <- atkDescFs]
        finishAtk   = Just $ finishAttack atk

        groundedAtkDescs =
            [ _punch1
            , _punch2
            , _doubleKick1
            , _doubleKick2
            , _punch3
            , _kick
            , _dashPunchForwards
            , _dashPunch
            , _diveKickLand
            , _palmExplodeRelease
            , _groundPunchRelease
            , _fallingAxeKickLand
            ]

        partialGroundedAtkDescs =
            [ _risingUppercut
            , _evasiveKick
            , _flyingKickRelease
            ]

drawGauntletsOverlay :: (GraphicsReadWrite m, MonadIO m) => WeaponDrawOverlay GauntletsData m
drawGauntletsOverlay wpnDrawOverlayStatus player atk gauntlets
    | _useChargeFlashIndicator cfg = drawGauntletsChargeFlashIndicator player atk gauntlets
    | otherwise                    = drawGauntletsChargeOverlay wpnDrawOverlayStatus player gauntlets
    where cfg = _config (_data gauntlets :: GauntletsData)

drawGauntletsChargeFlashIndicator
    :: (GraphicsReadWrite m, MonadIO m)
    => Player
    -> Maybe Attack
    -> Weapon GauntletsData
    -> m ()
drawGauntletsChargeFlashIndicator player atk gauntlets = do
    lerpOffset <- playerLerpOffset player
    sequenceA_ $ do
        spr  <- _overlaySprite $ _data gauntlets
        atk' <- atk
        guard $ releasableFrameTagName `isAttackFrameTag` atk'

        let
            atkPos = _pos (atk' :: Attack) `vecAdd` lerpOffset
            atkDir = _dir (atk' :: Attack)
        Just $ drawSpriteRotated atkPos atkDir playerWeaponOverlayZIndex (_angle atk') spr

drawGauntletsChargeOverlay
    :: (GraphicsReadWrite m, MonadIO m)
    => WeaponDrawOverlayStatus
    -> Player
    -> Weapon GauntletsData
    -> m ()
drawGauntletsChargeOverlay WeaponDrawOverlayBackground _ _              = return ()
drawGauntletsChargeOverlay WeaponDrawOverlayForeground player gauntlets =
    let
        gauntletsData    = _data gauntlets
        dir              = _dir (player :: Player)
        pos              = _pos (player :: Player)
        chargeOverlaySpr = _chargeOverlaySprite gauntletsData
        playerCfg        = _config (player :: Player)

        offset = fromMaybe (_chargeOverlaySprOffset playerCfg) $ do
            spr            <-
                playerAttackSprite player <|> playerMovementSkillSprite player <|> Just (_sprite (player :: Player))
            let sprFileName = takeBaseName $ _filePath (spr :: Sprite)
            offsets        <- M.lookup sprFileName (_chargeOverlaySprOffsetMap playerCfg)
            listToMaybe (drop (_int (_frameIndex spr :: FrameIndex)) offsets) <|> maybeLast offsets

        pos' = pos `vecAdd` (offset `vecFlip` dir)
    in when (isReleasableAtkConditionsMet gauntletsData) $ do
        lerpOffset <- playerLerpOffset player
        drawSprite (pos' `vecAdd` lerpOffset) dir playerUnderBodyZIndex chargeOverlaySpr
