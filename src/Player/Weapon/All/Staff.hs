module Player.Weapon.All.Staff
    ( mkStaffWeapon
    ) where

import Control.Applicative    ((<|>))
import Control.Monad          (when)
import Control.Monad.State    (execState, modify)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))
import Data.Maybe             (fromMaybe)

import Attack
import Attack.Projectile
import Configs
import Configs.All.PlayerWeapon
import Configs.All.PlayerWeapon.Staff
import Constants
import FileCache
import Id
import Msg
import Player
import Player.BufferedInputState
import Player.Weapon
import Player.Weapon.All.Staff.Data
import Player.Weapon.All.Staff.WindProjectile
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

impactFrameTag        = FrameTagName "impact"        :: FrameTagName
strike2LaunchFrameTag = FrameTagName "strike2Launch" :: FrameTagName
strike3LaunchFrameTag = FrameTagName "strike3Launch" :: FrameTagName
soundFrameTag         = FrameTagName "sound"         :: FrameTagName
endFrameTag           = FrameTagName "end"           :: FrameTagName

chargeOverlaySoundPath       = "event:/SFX Events/Player/Weapons/Staff/charge-overlay-c" :: FilePath
parabolicStrikeSoundFilePath = "event:/SFX Events/Player/Weapons/Staff/parabolic-strike" :: FilePath

mkStaffWeapon :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some Weapon)
mkStaffWeapon = do
    staffData <- mkStaffData
    return . Some $ (mkWeapon staffData StaffWeapon)
        { _think       = thinkStaff
        , _update      = updateStaff
        , _drawOverlay = drawStaffChargeOverlay
        }

thinkStaff :: (InputRead m, MonadIO m) => WeaponThink StaffData m
thinkStaff weaponThinkStatus player currentAtk staff = do
    let staffData = _data staff

    inputState <- readInputState

    atkFromInput <- case weaponThinkStatus of
        WeaponThinkForeground WeaponAttackReady -> mkAttackFromInput staffData inputState player currentAtk
        _                                       -> return Nothing

    newAtk <- case atkFromInput of
        Just _  -> return atkFromInput
        Nothing -> maybe (return Nothing) (alterStaffAttack staffData inputState player) currentAtk

    let
        atk        = newAtk <|> currentAtk
        msgs       = thinkStaffAttack staffData atk player
        newAtkMsgs = maybe [] (pure . mkMsg . PlayerMsgSetAttack) newAtk
    chargeMsgs <- thinkStaffCharge staffData weaponThinkStatus player atk

    return $ newAtkMsgs ++ chargeMsgs ++ msgs

thinkStaffCharge
    :: InputRead m
    => StaffData
    -> WeaponThinkStatus
    -> Player
    -> Maybe Attack
    -> m [Msg ThinkPlayerMsgsPhase]
thinkStaffCharge staffData wpnThinkStatus player atk = (chargeAudioMsgs ++) <$> chargeInputMsgs
    where
        chargeStatus    = _chargeStatus staffData
        hashedId        = _chargeSoundHashedId staffData
        pos             = _pos (player :: Player)
        chargeAudioMsgs = case chargeStatus of
            StaffNoChargeStatus        -> []
            StaffPartialChargeStatus _ -> []
            StaffFullChargeStatus      -> case wpnThinkStatus of
                WeaponThinkForeground _ ->
                    [ mkMsg $ AudioMsgMuteSound hashedId False
                    , mkMsg $ AudioMsgPlaySoundContinuous chargeOverlaySoundPath hashedId pos
                    ]
                WeaponThinkBackground   ->
                    [ mkMsg $ AudioMsgMuteSound hashedId True
                    , mkMsg $ AudioMsgPlaySoundContinuous chargeOverlaySoundPath hashedId pos
                    ]

        chargeInputMsgs = case wpnThinkStatus of
            WeaponThinkBackground   -> return []
            WeaponThinkForeground _ -> case chargeStatus of
                StaffNoChargeStatus        -> return []
                StaffPartialChargeStatus _ -> return []
                StaffFullChargeStatus      -> readInputState <&> \inputState ->
                    let
                        cfg                    = _config (staffData :: StaffData)
                        chargeMeterCost        = _chargeMeterCost cfg
                        weaponNotHeld          = not $ WeaponAlias `aliasHold` inputState
                        parabolicStrikeAtkDesc = _parabolicStrike $ _attackDescriptions staffData
                        isParabolicStrikeAtk   = maybe False (`attackIs` parabolicStrikeAtkDesc) atk
                    in if
                        | _prevChargeStatus staffData /= StaffFullChargeStatus ->
                            [mkMsg (PlayerMsgSpendMeter chargeMeterCost)]
                        | weaponNotHeld && not isParabolicStrikeAtk            ->
                            -- refund meter if released button during uncancelable action and there's no attack
                            [mkMsg $ PlayerMsgGainMeter NullId chargeMeterCost]
                        | otherwise                                            -> []

thinkStaffAttack :: StaffData -> Maybe Attack -> Player -> [Msg ThinkPlayerMsgsPhase]
thinkStaffAttack staffData atk player = flip execState [] $ case atk of
    Nothing   -> return ()
    Just atk' ->
        let
            isFrameTag = \frameTag -> frameTag `isSpriteFrameTag` attackSprite atk'

            pos@(Pos2 x y) = _pos (player :: Player)
            cfg            = _config (staffData :: StaffData)

            atkDesc                = _description atk'
            atkFrameChanged        = attackFrameChanged atk'
            atkDescs               = _attackDescriptions staffData
            groundStrikeAtkDesc    = _groundStrike atkDescs
            parabolicStrikeAtkDesc = _parabolicStrike atkDescs
        in do
            when (atkDesc == groundStrikeAtkDesc && isFrameTag impactFrameTag && atkFrameChanged) $
                let
                    dir                 = _dir (player :: Player)
                    groundStrikeOffsetX = _groundStrikeOffsetX cfg
                    offsetPos           = Pos2 (x + groundStrikeOffsetX * directionNeg dir) y
                    mkEffectAtkProj     = mkPlayerAttackProjectile offsetPos dir (_groundStrikeEffect atkDescs)
                    mkWindProj          = mkWindProjectile offsetPos dir (_windProjectileAppearIdle atkDescs)

                    windProjMeterCost   = _windProjectileMeterCost cfg
                    canSpendMeter       = canSpendPlayerMeter windProjMeterCost player
                    windProjMsgs
                        | canSpendMeter =
                            [ mkMsg $ NewThinkProjectileMsgAddM mkWindProj
                            , mkMsg $ PlayerMsgSpendMeter windProjMeterCost
                            , mkMsg $ NewThinkProjectileMsgAddM mkEffectAtkProj
                            ]
                        | otherwise     = [mkMsg $ UiMsgInsufficientMeter windProjMeterCost False]
                in modify $ (windProjMsgs ++)

            when (atkDesc == parabolicStrikeAtkDesc && isFrameTag soundFrameTag && atkFrameChanged) $
                let parabolicStrikeSoundMsg = mkMsg $ AudioMsgPlaySound parabolicStrikeSoundFilePath pos
                in modify (parabolicStrikeSoundMsg:)

mkAttackFromInput :: MonadIO m => StaffData -> InputState -> Player -> Maybe Attack -> m (Maybe Attack)
mkAttackFromInput staffData inputState player atk
    -- dash launch strike (left)
    | (inPlayerTapInputBuffer [LeftInput, LeftInput] player || isPlayerInputBufferQCF LeftDir player) &&
      not rightHeld && weaponPressed && onGround && attackIsNot dashLaunchStrike =
        Just <$> mkAttack pos LeftDir dashLaunchStrike

    -- dash launch strike (right)
    | (inPlayerTapInputBuffer [RightInput, RightInput] player || isPlayerInputBufferQCF RightDir player) &&
      not leftHeld && weaponPressed && onGround && attackIsNot dashLaunchStrike =
        Just <$> mkAttack pos RightDir dashLaunchStrike

    -- up spin strike1
    | onGround && weaponUpPressed && attackNotIn [upSpinStrike1, upSpinStrike2, upSpinStrike3] =
        mkAttack' upSpinStrike1

    -- ground strike
    | onGround && weaponDownPressed && attackIsNot groundStrike = mkAttack' groundStrike

    | onGround && weaponPressed = if
        -- strike3-launch
        | attackIn' [strike2, strike2Launch] && attackIsFrameTag strike3LaunchFrameTag -> mkAttack' strike3Launch
        -- strike3
        | attackIn' [strike2, strike2Launch] && atkCancelable                          -> mkAttack' strike3
        -- strike2-launch
        | attackIs' strike1 && attackIsFrameTag strike2LaunchFrameTag                  -> mkAttack' strike2Launch
        -- strike2
        | attackIs' strike1 && atkCancelable                                           -> mkAttack' strike2
        -- strike1
        | otherwise                                                                    -> if
            | attackNotIn [strike3, strike3Launch] -> mkAttack' strike1
            | otherwise                            -> return Nothing

    -- air lift strike
    | inAir && weaponDownPressed && attackIsNot airLiftStrike = mkAttack' airLiftStrike

    -- air strike
    | inAir && weaponPressed && attackIsNot airStrike =
        Just . updateAirStrikeOnHit staffData <$> mkAttack pos dir airStrike

    -- parabolic strike
    | weaponNotHeld && _chargeStatus staffData == StaffFullChargeStatus = mkAttack' parabolicStrike

    | otherwise = return Nothing

    where
        attackIs'        = \atkDesc -> maybe False (`attackIs` atkDesc) atk
        attackIsNot      = \atkDesc -> not $ attackIs' atkDesc
        attackIn'        = \atkDescs -> maybe False (`attackIn` atkDescs) atk
        attackNotIn      = \atkDescs -> not $ attackIn' atkDescs
        attackIsFrameTag = \frameTagName -> maybe False (isSpriteFrameTag frameTagName . attackSprite) atk

        pos           = _pos (player :: Player)
        onGround      = _touchingGround (_flags player)
        inAir         = not onGround
        atkCancelable = fromMaybe True (attackCancelable <$> atk)

        wpnAtkDescs      = _attackDescriptions staffData
        strike1          = _strike1 wpnAtkDescs
        strike2          = _strike2 wpnAtkDescs
        strike2Launch    = _strike2Launch wpnAtkDescs
        strike3          = _strike3 wpnAtkDescs
        strike3Launch    = _strike3Launch wpnAtkDescs
        upSpinStrike1    = _upSpinStrike1 wpnAtkDescs
        upSpinStrike2    = _upSpinStrike2 wpnAtkDescs
        upSpinStrike3    = _upSpinStrike3 wpnAtkDescs
        groundStrike     = _groundStrike wpnAtkDescs
        dashLaunchStrike = _dashLaunchStrike wpnAtkDescs
        airStrike        = _airStrike wpnAtkDescs
        airLiftStrike    = _airLiftStrike wpnAtkDescs
        parabolicStrike  = _parabolicStrike wpnAtkDescs

        leftHeld        = LeftAlias `aliasHold` inputState
        rightHeld       = RightAlias `aliasHold` inputState
        dir
            | leftHeld  = LeftDir
            | rightHeld = RightDir
            | otherwise = _dir (player :: Player)

        mkAttack' = \atkDesc -> Just <$> mkAttack pos dir atkDesc

        weaponPressed     = WeaponAlias `aliasPressed` inputState || WeaponInput `inPlayerInputBuffer` player
        weaponUpPressed   = (WeaponAlias `aliasPressed` inputState && UpAlias `aliasHold` inputState) ||
            WeaponUpInput `inPlayerInputBuffer` player
        weaponDownPressed =
            (WeaponAlias `aliasPressed` inputState && DownAlias `aliasHold` inputState) ||
            WeaponDownInput `inPlayerInputBuffer` player
        weaponNotHeld     = not $ WeaponAlias `aliasHold` inputState

alterStaffAttack :: MonadIO m => StaffData -> InputState -> Player -> Attack -> m (Maybe Attack)
alterStaffAttack staffData _ player atk
    -- parabolic strike -> parabolic strike land
    | attackIs' _parabolicStrike && onGround && velY > 0.0 = mkAttack' _parabolicStrikeLand

    -- air strike -> cancel attack on land
    | attackIs' _airStrike && onGround && velY >= 0.0 = return finishAtk

    -- air lift strike -> cancel attack on land
    | attackIs' _airLiftStrike && onGround = return finishAtk

    -- dash launch strike -> cancel attack if ending in air
    | attackIs' _dashLaunchStrike && not onGround && endFrameTag `isAttackFrameTag` atk = return finishAtk

    -- cancel grounded attacks if now in air
    | not onGround && attackIn' groundedAtkDescs = return finishAtk

    | otherwise = return Nothing

    where
        pos         = _pos (player :: Player)
        dir         = _dir (player :: Player)
        velY        = vecY $ _vel (player :: Player)
        onGround    = _touchingGround $ _flags player
        finishAtk   = Just $ finishAttack atk
        wpnAtkDescs = _attackDescriptions staffData

        mkAttack' = \atkDescF -> Just <$> mkAttack pos dir (atkDescF wpnAtkDescs)
        attackIs'   = \atkDescF -> atk `attackIs` (atkDescF wpnAtkDescs)
        attackIn'   = \atkDescFs -> atk `attackIn` [atkDescF wpnAtkDescs | atkDescF <- atkDescFs]

        groundedAtkDescs =
            [ _strike1
            , _strike2
            , _strike2Launch
            , _strike3
            , _strike3Launch
            , _upSpinStrike1
            , _upSpinStrike2
            , _upSpinStrike3
            , _groundStrike
            , _parabolicStrikeLand
            ]

updateStaff :: (ConfigsRead m, InputRead m, MsgsWrite UpdatePlayerMsgsPhase m) => WeaponUpdate StaffData m
updateStaff WeaponUpdateBackground _ _ staff      = return staff
updateStaff WeaponUpdateForeground player _ staff = do
    inputState <- readInputState

    let
        onGround        = _touchingGround (_flags player)
        staffData       = _data staff
        airStrikeCount
            | onGround  = 0
            | otherwise = _airStrikeCount staffData

        cfg                     = _config (staffData :: StaffData)
        chargeHeldThresholdSecs = _chargeHeldThresholdSecs cfg

        weaponHeld      = WeaponAlias `aliasHold` inputState
        chargeMeterCost = _chargeMeterCost cfg
        canSpendMeter   = canSpendPlayerMeter chargeMeterCost player

        chargeStatus  = _chargeStatus staffData
        chargeStatus' = case chargeStatus of
            StaffNoChargeStatus
                | weaponHeld -> StaffPartialChargeStatus timeStep
            StaffPartialChargeStatus chargeSecs
                | weaponHeld ->
                    let chargeSecs' = chargeSecs + timeStep
                    in if
                        | chargeSecs' >= chargeHeldThresholdSecs && canSpendMeter -> StaffFullChargeStatus
                        | otherwise                                               ->
                            StaffPartialChargeStatus chargeSecs'
            StaffFullChargeStatus
                | weaponHeld -> StaffFullChargeStatus
            _                -> StaffNoChargeStatus

    let chargeAboveThreshold = staffChargeStatusChargeSecs chargeStatus' >= chargeHeldThresholdSecs
    when (chargeAboveThreshold && chargeStatus' /= StaffFullChargeStatus && not canSpendMeter) $
        writeMsgs [mkMsg $ UiMsgInsufficientMeter chargeMeterCost False]

    staffData' <- readConfig _playerWeapon _staff <&> \cfg' -> staffData
        { _airStrikeCount   = airStrikeCount
        , _prevChargeStatus = chargeStatus
        , _chargeStatus     = chargeStatus'
        , _chargeOverlaySpr = updateSprite $ _chargeOverlaySpr staffData
        , _config           = cfg'
        }

    return $ staff {_data = staffData'}

drawStaffChargeOverlay :: (GraphicsReadWrite m, MonadIO m) => WeaponDrawOverlay StaffData m
drawStaffChargeOverlay WeaponDrawOverlayBackground _ _ _          = return ()
drawStaffChargeOverlay WeaponDrawOverlayForeground player _ staff =
    let
        staffData        = _data staff
        dir              = _dir (player :: Player)
        pos              = _pos (player :: Player)
        chargeOverlaySpr = _chargeOverlaySpr staffData
        offset           = playerChargeOverlayOffset player
        pos'             = pos `vecAdd` (offset `vecFlip` dir)
    in when (_chargeStatus staffData == StaffFullChargeStatus) $ do
        lerpOffset <- playerLerpOffset player
        drawSprite (pos' `vecAdd` lerpOffset) dir playerWeaponOverlayZIndex chargeOverlaySpr
