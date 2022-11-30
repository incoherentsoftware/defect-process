module Player.Weapon.All.SpiritBlade
    ( mkSpiritBladeWeapon
    ) where

import Control.Applicative    ((<|>))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execState, execStateT, get, lift, modify, put)
import Data.Functor           ((<&>))
import qualified Data.Map as M
import qualified Data.Set as S

import Attack
import Configs
import Configs.All.PlayerWeapon
import Configs.All.PlayerWeapon.SpiritBlade
import Constants
import FileCache
import Id
import Msg
import Player
import Player.BufferedInputState
import Player.Weapon as W
import Player.Weapon.All.SpiritBlade.CheckWallOffsetDummyProj
import Player.Weapon.All.SpiritBlade.Data
import Player.Weapon.All.SpiritBlade.SpiritFormProjectile
import Player.Weapon.All.SpiritBlade.Util
import Projectile as P
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

spiritAppearSoundPath   = "event:/SFX Events/Player/Weapons/Spirit Blade/spirit-appear"   :: FilePath
spiritActivateSoundPath = "event:/SFX Events/Player/Weapons/Spirit Blade/spirit-activate" :: FilePath

summonFrameTagName          = FrameTagName "summon"          :: FrameTagName
checkWallOffsetFrameTagName = FrameTagName "checkWallOffset" :: FrameTagName
activateEffectOffset        = Pos2 0.0 (-90.0)               :: Pos2
spiritFormAliveTimeoutSecs  = 0.02                           :: Secs

mkSpiritBladeWeapon :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some Weapon)
mkSpiritBladeWeapon = do
    spiritBladeData <- mkSpiritBladeData
    return . Some $ (mkWeapon spiritBladeData SpiritBladeWeapon)
        { _think       = thinkSpiritBlade
        , _update      = updateSpiritBlade
        , _drawOverlay = drawSpiritBladeChargeOverlay
        }

thinkSpiritBladeUtilMsgs
    :: InputRead m
    => SpiritBladeData
    -> WeaponThinkStatus
    -> Maybe Attack
    -> Maybe Attack
    -> m [Msg ThinkPlayerMsgsPhase]
thinkSpiritBladeUtilMsgs spiritBladeData weaponThinkStatus currentAtk atkFromInput =
    let
        setIsInputHeldForSpiritForm :: Bool -> Weapon SpiritBladeData -> Weapon SpiritBladeData
        setIsInputHeldForSpiritForm isInputHeld spiritBlade = spiritBlade
            { _data = (W._data spiritBlade) {_isInputHeldForSpiritForm = isInputHeld}
            }
    in do
        isInputHeldForSpiritForm <- case atkFromInput of
            Just _  -> return True
            Nothing -> readInputState <&> \inputState -> case weaponThinkStatus of
                WeaponThinkForeground _
                    | not $ WeaponAlias `aliasHold` inputState -> False
                _                                              -> _isInputHeldForSpiritForm spiritBladeData

        let
            setIsInputHeldMsg   = mkMsg $ PlayerMsgUpdateWeapon (setIsInputHeldForSpiritForm isInputHeldForSpiritForm)
            checkWallOffsetMsgs = case atkFromInput <|> currentAtk of
                Just atk
                    | checkWallOffsetFrameTagName `isAttackFrameTag` atk && attackFrameChanged atk ->
                        [mkMsg $ NewThinkProjectileMsgAddM (mkCheckWallOffsetDummyProj atk)]
                _                                                                                  -> []

        return $ setIsInputHeldMsg:checkWallOffsetMsgs

thinkSpiritBlade :: (MsgsRead ThinkPlayerMsgsPhase m, InputRead m, MonadIO m) => WeaponThink SpiritBladeData m
thinkSpiritBlade weaponThinkStatus player currentAtk spiritBlade = do
    let spiritBladeData = W._data spiritBlade

    spiritFormAtkMsgs <- case weaponThinkStatus of
        WeaponThinkForeground _ -> thinkSpiritFormAttackMsgs spiritBladeData player
        WeaponThinkBackground   -> return []

    newSpiritFormMsgs <- case weaponThinkStatus of
        WeaponThinkForeground _ -> thinkNewSpiritFormMsgs spiritBladeData currentAtk player
        WeaponThinkBackground   -> return []

    atkFromInput <- case weaponThinkStatus of
        WeaponThinkForeground WeaponAttackReady
            | null spiritFormAtkMsgs && null newSpiritFormMsgs -> mkAttackFromInput spiritBladeData player currentAtk
        _                                                      -> return Nothing

    utilMsgs <- thinkSpiritBladeUtilMsgs spiritBladeData weaponThinkStatus currentAtk atkFromInput

    let
        newAtk     = case atkFromInput of
            Just _  -> atkFromInput
            Nothing -> maybe Nothing (alterSpiritBladeAttack spiritBladeData player) currentAtk
        setAtkMsgs = maybe [] (pure . mkMsg . PlayerMsgSetAttack) newAtk

    return $ spiritFormAtkMsgs ++ newSpiritFormMsgs ++ utilMsgs ++ setAtkMsgs

thinkSpiritFormAttackMsgs :: InputRead m => SpiritBladeData -> Player -> m [Msg ThinkPlayerMsgsPhase]
thinkSpiritFormAttackMsgs spiritBladeData player = fromInput <$> readInputState
    where
        onGround              = _touchingGround (_flags player)
        inAir                 = not onGround
        spiritFormMsgsIds     = _spiritFormMsgIds spiritBladeData
        largeSlashMsgId       = _largeSlashId spiritFormMsgsIds
        launchSlashMsgId      = _launchSlashId spiritFormMsgsIds
        backwardsSlashMsgId   = _backwardsSlashId spiritFormMsgsIds
        pillarBlastMsgId      = _pillarBlastId spiritFormMsgsIds
        airBlowMsgId          = _airBlowId spiritFormMsgsIds
        airCircularSlashMsgId = _airCircularSlashId spiritFormMsgsIds

        commonMsgs :: MsgId -> (SpiritFormMsgIds -> SpiritFormMsgIds) -> [Msg ThinkPlayerMsgsPhase]
        commonMsgs spiritFormMsgId updateSpiritFormMsgIds =
            [ mkMsgTo (ProjectileMsgUpdate setSpiritFormProjectileTriggered) spiritFormMsgId
            , mkMsg $ PlayerMsgUpdateWeapon updateWpn
            , mkMsg $ PlayerMsgClearInputBuffer allWeaponBufferedInputs
            , mkMsg $ AudioMsgPlaySound spiritActivateSoundPath pos
            ]
            where
                pos       = _pos (player :: Player)
                updateWpn = \sb ->
                    let sbData = W._data sb
                    in sb
                        { W._data = sbData
                            { _overlaySprite    = Just $ _activateEffect (_sprites (sbData :: SpiritBladeData))
                            , _spiritFormMsgIds = updateSpiritFormMsgIds $ _spiritFormMsgIds sbData
                            }
                        }

        fromInput :: InputState -> [Msg ThinkPlayerMsgsPhase]
        fromInput inputState
            -- spirit pillar blast
            | pillarBlastMsgId /= NullId && onGround && weaponPressed && isLeftOrRightDirSpecial =
                let updateSpiritFormMsgIds = \spiritFormMsgIds -> spiritFormMsgIds {_pillarBlastId = NullId}
                in commonMsgs pillarBlastMsgId updateSpiritFormMsgIds

            -- spirit launch slash
            | launchSlashMsgId /= NullId && onGround && weaponUpPressed =
                let updateSpiritFormMsgIds = \spiritFormMsgIds -> spiritFormMsgIds {_launchSlashId = NullId}
                in commonMsgs launchSlashMsgId updateSpiritFormMsgIds

            -- spirit backwards slash
            | backwardsSlashMsgId /= NullId && onGround && weaponDownPressed =
                let updateSpiritFormMsgIds = \spiritFormMsgIds -> spiritFormMsgIds {_backwardsSlashId = NullId}
                in commonMsgs backwardsSlashMsgId updateSpiritFormMsgIds

            -- spirit large slash
            | largeSlashMsgId /= NullId && onGround && weaponPressed &&
              not (downHold || upHold || isLeftOrRightDirSpecial) =
                let updateSpiritFormMsgIds = \spiritFormMsgIds -> spiritFormMsgIds {_largeSlashId = NullId}
                in commonMsgs largeSlashMsgId updateSpiritFormMsgIds

            -- spirit air blow
            | airBlowMsgId /= NullId && inAir && weaponPressed && not downHold =
                let updateSpiritFormMsgIds = \spiritFormMsgIds -> spiritFormMsgIds {_airBlowId = NullId}
                in commonMsgs airBlowMsgId updateSpiritFormMsgIds

            -- spirit air circular slash
            | airCircularSlashMsgId /= NullId && inAir && weaponDownPressed =
                let updateSpiritFormMsgIds = \spiritFormMsgIds -> spiritFormMsgIds {_airCircularSlashId = NullId}
                in commonMsgs airCircularSlashMsgId updateSpiritFormMsgIds

            | otherwise = []

            where
                upHold            = UpAlias `aliasHold` inputState
                downHold          = DownAlias `aliasHold` inputState
                weaponPressed     = WeaponAlias `aliasPressed` inputState || WeaponInput `inPlayerInputBuffer` player
                weaponUpPressed   =
                    (WeaponAlias `aliasPressed` inputState && upHold) || WeaponDownInput `inPlayerInputBuffer` player
                weaponDownPressed =
                    (WeaponAlias `aliasPressed` inputState && downHold) || WeaponDownInput `inPlayerInputBuffer` player

                isLeftOrRightDirSpecial =
                    inPlayerTapInputBuffer [LeftInput, LeftInput] player || isPlayerInputBufferQCF LeftDir player ||
                    inPlayerTapInputBuffer [RightInput, RightInput] player || isPlayerInputBufferQCF RightDir player

thinkNewSpiritFormMsgs
    :: (MsgsRead ThinkPlayerMsgsPhase m, MonadIO m)
    => SpiritBladeData
    -> Maybe Attack
    -> Player
    -> m [Msg ThinkPlayerMsgsPhase]
thinkNewSpiritFormMsgs _ Nothing _                       = return []
thinkNewSpiritFormMsgs spiritBladeData (Just atk) player =
    let
        isSummonFrame       = summonFrameTagName `isAttackFrameTag` atk
        atkId               = _id (atk :: Attack)
        notPrevSummoned     = atkId /= _lastSummonAttackId spiritBladeData
        canSummon           = isSummonFrame && notPrevSummoned && _isInputHeldForSpiritForm spiritBladeData
        spiritFormMeterCost = _spiritFormMeterCost $ _config (spiritBladeData :: SpiritBladeData)
        canSpendMeter       = canSpendPlayerMeter spiritFormMeterCost player
    in case attackToSpiritFormData spiritBladeData atk of
        Just spiritFormData
            | canSummon -> if
                | not canSpendMeter -> return [mkMsg $ UiMsgInsufficientMeter spiritFormMeterCost False]
                | otherwise         -> do
                    let
                        dir                = _dir (player :: Player)
                        checkedWallOffsetX = _checkedWallOffsetX spiritBladeData
                    groundBeneathOffsetY <- readPlayerGroundBeneathOffsetY

                    let
                        offset     = (_offset spiritFormData) dir checkedWallOffsetX groundBeneathOffsetY
                        pos        = _pos (player :: Player) `vecAdd` offset
                        atkDescs   = _attackDescriptions spiritBladeData
                        appearAtkF = _appearAtkDesc spiritFormData
                        atksF      = _atkDescs spiritFormData
                    Some spiritFormProj <- mkSpiritFormProjectile pos dir appearAtkF atksF atkDescs

                    let
                        updateWpn = \sb ->
                            let
                                sbData                 = W._data sb
                                updateSpiritFormMsgIds = _updateSpiritFormMsgIds spiritFormData
                            in sb
                                { W._data = sbData
                                    { _lastSummonAttackId = atkId
                                    , _spiritFormMsgIds   =
                                        updateSpiritFormMsgIds (P._msgId spiritFormProj) (_spiritFormMsgIds sbData)
                                    }
                                }

                    return
                        [ mkMsg $ NewThinkProjectileMsgAdd (Some spiritFormProj)
                        , mkMsg $ PlayerMsgUpdateWeapon updateWpn
                        , mkMsgTo (ProjectileMsgSetTtl 0.0) (_existingMsgId spiritFormData)
                        , mkMsg $ PlayerMsgSpendMeter spiritFormMeterCost
                        , mkMsg $ AudioMsgPlaySound spiritAppearSoundPath pos
                        ]

        _ -> return []

mkAttackFromInput :: (InputRead m, MonadIO m) => SpiritBladeData -> Player -> Maybe Attack -> m (Maybe Attack)
mkAttackFromInput spiritBladeData player atk = fromInput =<< readInputState
    where
        fromInput :: MonadIO m1 => InputState -> m1 (Maybe Attack)
        fromInput inputState
            -- advancing slash (left)
            | (inPlayerTapInputBuffer [LeftInput, LeftInput] player || isPlayerInputBufferQCF LeftDir player) &&
              not rightHeld && weaponPressed && onGround && isNotAdvancingSlash && pillarBlastMsgId == NullId =
                Just <$> mkAttack pos LeftDir advancingSlash

            -- advancing slash (right)
            | (inPlayerTapInputBuffer [RightInput, RightInput] player || isPlayerInputBufferQCF RightDir player) &&
              not leftHeld && weaponPressed && onGround && isNotAdvancingSlash && pillarBlastMsgId == NullId =
                Just <$> mkAttack pos RightDir advancingSlash

            -- knockback slash
            | weaponDownPressed && onGround && attackIsNot knockbackSlash && backwardsSlashMsgId == NullId =
                mkAttack' knockbackSlash

            -- spin slash
            | weaponUpPressed && onGround && isNotSpinSlash && launchSlashMsgId == NullId =
                mkAttack' spinSlash

            -- air down thrust
            | inAir && weaponDownPressed && attackIsNot airDownThrust && airCircularSlashMsgId == NullId =
                mkAttack' airDownThrust

            -- air double slash
            | inAir && weaponPressed && isNotAirDoubleSlash && airBlowMsgId == NullId =
                mkAttack' airDoubleSlash1

            -- double slash
            | onGround && weaponPressed && isNotDoubleSlash && largeSlashMsgId == NullId =
                mkAttack' doubleSlash1

            | otherwise = return Nothing

            where
                leftHeld        = LeftAlias `aliasHold` inputState
                rightHeld       = RightAlias `aliasHold` inputState
                dir
                    | leftHeld  = LeftDir
                    | rightHeld = RightDir
                    | otherwise = _dir (player :: Player)

                pos           = _pos (player :: Player)
                mkAttack'     = \atkDesc -> Just <$> mkAttack pos dir atkDesc
                attackIsNot   = \atkDesc -> not $ maybe False (`attackIs` atkDesc) atk
                attackNotIn   = \atkDescs -> and $ map attackIsNot atkDescs

                weaponPressed     = WeaponAlias `aliasPressed` inputState || WeaponInput `inPlayerInputBuffer` player
                weaponDownPressed =
                    (WeaponAlias `aliasPressed` inputState && DownAlias `aliasHold` inputState) ||
                    WeaponDownInput `inPlayerInputBuffer` player
                weaponUpPressed   =
                    (WeaponAlias `aliasPressed` inputState && UpAlias `aliasHold` inputState) ||
                    WeaponUpInput `inPlayerInputBuffer` player

                onGround = _touchingGround $ _flags player
                inAir    = not onGround

                spiritFormMsgsIds     = _spiritFormMsgIds spiritBladeData
                largeSlashMsgId       = _largeSlashId spiritFormMsgsIds
                launchSlashMsgId      = _launchSlashId spiritFormMsgsIds
                backwardsSlashMsgId   = _backwardsSlashId spiritFormMsgsIds
                pillarBlastMsgId      = _pillarBlastId spiritFormMsgsIds
                airCircularSlashMsgId = _airCircularSlashId spiritFormMsgsIds
                airBlowMsgId          = _airBlowId spiritFormMsgsIds

                wpnAtkDescs            = _attackDescriptions spiritBladeData
                advancingSlash         = _advancingSlash wpnAtkDescs
                advancingSlashFollowup = _advancingSlashFollowup wpnAtkDescs
                knockbackSlash         = _knockbackSlash wpnAtkDescs
                spinSlash              = _spinSlash wpnAtkDescs
                airDoubleSlash1        = _airDoubleSlash1 wpnAtkDescs
                airDoubleSlash2        = _airDoubleSlash2 wpnAtkDescs
                airDownThrust          = _airDownThrust wpnAtkDescs
                doubleSlash1           = _doubleSlash1 wpnAtkDescs
                doubleSlash2           = _doubleSlash2 wpnAtkDescs

                isNotAdvancingSlash = attackNotIn [advancingSlash, advancingSlashFollowup]
                isNotSpinSlash      = attackNotIn [spinSlash]
                isNotAirDoubleSlash = attackNotIn [airDoubleSlash1, airDoubleSlash2]
                isNotDoubleSlash    = attackNotIn [doubleSlash1, doubleSlash2]

alterSpiritBladeAttack :: SpiritBladeData -> Player -> Attack -> Maybe Attack
alterSpiritBladeAttack spiritBladeData player atk
    -- air down thrust land
    | attackIs' _airDownThrust && onGround = Just finishedAtk

    -- air double slash land
    | attackIn' [_airDoubleSlash1, _airDoubleSlash2] && onGround = Just finishedAtk

    -- cancel grounded attacks if now in air
    | not onGround && attackIn' groundedAtkDescs = Just finishedAtk

    | otherwise = Nothing

    where
        wpnAtkDescs = _attackDescriptions spiritBladeData
        attackIs'   = \atkDescF -> attackIs atk (atkDescF wpnAtkDescs)
        attackIn'   = \atkDescsF -> attackIn atk (map ($ wpnAtkDescs) atkDescsF)
        onGround    = _touchingGround $ _flags player
        finishedAtk = finishAttack atk

        groundedAtkDescs =
            [ _doubleSlash1
            , _doubleSlash2
            , _knockbackSlash
            , _spinSlash
            , _advancingSlash
            , _advancingSlashFollowup
            ]

updateSpiritBladeOverlaySprite :: Weapon SpiritBladeData -> Weapon SpiritBladeData
updateSpiritBladeOverlaySprite spiritBlade = spiritBlade {W._data = spiritBladeData'}
    where
        spiritBladeData  = W._data spiritBlade
        overlaySpr       = case _overlaySprite spiritBladeData of
            Just spr
                | not (spriteFinished spr) -> Just $ updateSprite spr
            _                              -> Nothing
        spiritBladeData' = spiritBladeData {_overlaySprite = overlaySpr}

clearInactiveSpiritFormIds :: MsgsRead UpdatePlayerMsgsPhase m => Weapon SpiritBladeData -> m (Weapon SpiritBladeData)
clearInactiveSpiritFormIds spiritBlade =
    let
        processInfoMsg :: InfoMsgPayload -> S.Set MsgId -> S.Set MsgId
        processInfoMsg p allProjIds = case p of
            InfoMsgProjectilePos _ _ msgId -> msgId `S.insert` allProjIds
            _                              -> allProjIds

        checkIdTimeout
            :: S.Set MsgId
            -> (SpiritFormMsgIds -> MsgId, SpiritFormMsgIds -> SpiritFormMsgIds)
            -> SpiritBladeData
            -> SpiritBladeData
        checkIdTimeout allProjIds (spiritIdF, clearSpiritId) sbData
            | spiritId `S.notMember` allProjIds = if
                | timeout >= spiritFormAliveTimeoutSecs -> sbData {_spiritFormMsgIds = clearSpiritId sfmi}
                | otherwise                             ->
                    let sfmiTimeouts' = M.insert spiritId (timeout + timeStep) sfmiTimeouts
                    in sbData {_spiritFormMsgIdTimeouts = sfmiTimeouts'}
            | otherwise                         = sbData
            where
                sfmi         = _spiritFormMsgIds sbData
                spiritId     = spiritIdF sfmi
                sfmiTimeouts = _spiritFormMsgIdTimeouts sbData
                timeout      = M.findWithDefault 0.0 spiritId sfmiTimeouts

        allSpiritIdsData =
            [ (_largeSlashId, \sfmi -> sfmi {_largeSlashId = NullId})
            , (_launchSlashId, \sfmi -> sfmi {_launchSlashId = NullId})
            , (_backwardsSlashId, \sfmi -> sfmi {_backwardsSlashId = NullId})
            , (_pillarBlastId, \sfmi -> sfmi {_pillarBlastId = NullId})
            , (_airBlowId, \sfmi -> sfmi {_airBlowId = NullId})
            , (_airCircularSlashId, \sfmi -> sfmi {_airCircularSlashId = NullId})
            ]
    in do
        allProjIds <- foldr processInfoMsg S.empty <$> readMsgs
        let
            spiritBladeData = flip execState (W._data spiritBlade) $ do
                modify $ \sbData -> foldr (checkIdTimeout allProjIds) sbData allSpiritIdsData
                whenM (isSpiritFormAllInactive <$> get) $
                    modify $ \sbData -> sbData {_spiritFormMsgIdTimeouts = M.empty}

        return $ spiritBlade {W._data = spiritBladeData}

updateSpiritBlade :: (ConfigsRead m, MsgsRead UpdatePlayerMsgsPhase m) => WeaponUpdate SpiritBladeData m
updateSpiritBlade WeaponUpdateBackground _ _ spiritBlade = return $ updateSpiritBladeOverlaySprite spiritBlade
updateSpiritBlade WeaponUpdateForeground _ _ spiritBlade = flip execStateT spiritBlade $ do
    modify updateSpiritBladeOverlaySprite
    get >>= lift . clearInactiveSpiritFormIds >>= put

    cfg <- lift $ readConfig _playerWeapon _spiritBlade
    modify $ \sb -> sb
        { W._data = (W._data sb) {_config = cfg} :: SpiritBladeData
        }

drawSpiritBladeChargeOverlay :: (GraphicsReadWrite m, MonadIO m) => WeaponDrawOverlay SpiritBladeData m
drawSpiritBladeChargeOverlay _ player _ spiritBlade = case _overlaySprite (W._data spiritBlade) of
    Nothing         -> return ()
    Just overlaySpr ->
        let
            pos = _pos (player :: Player) `vecAdd` activateEffectOffset
            dir = _dir (player :: Player)
        in drawSprite pos dir playerOverBodyZIndex overlaySpr
