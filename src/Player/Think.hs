module Player.Think
    ( thinkPlayer
    ) where

import Control.Monad       (when)
import Control.Monad.State (execState, execStateT, gets, lift, modify)
import Data.Maybe          (isNothing)
import qualified Data.Set as S

import AppEnv
import Attack
import Configs
import Configs.All.Settings
import Configs.All.Settings.Debug
import InfoMsg.Util
import Msg
import Player
import Player.BufferedInputState
import Player.Gun
import Player.Gun.Manager
import Player.LockOnAim
import Player.MovementSkill as MS
import Player.SecondarySkill as SS
import Player.SecondarySkill.Manager
import Player.TauntState
import Player.Weapon as W
import Player.Weapon.Manager
import Util
import Window.Graphics
import Window.InputState

invalidActionSoundPath = "event:/SFX Events/Level/pickup-item-cant-buy" :: FilePath

tauntActivateFrameTagName = FrameTagName "tauntActivate" :: FrameTagName
preTauntFrameTagName      = FrameTagName "preTaunt"      :: FrameTagName

playerInteractMsgs :: InputState -> Player -> [Msg ThinkPlayerMsgsPhase]
playerInteractMsgs inputState player
    | InteractAlias `aliasPressed` inputState = [mkMsg $ PlayerMsgInteract (_gold player)]
    | otherwise                               = []

canPlayerAttack :: Player -> Bool
canPlayerAttack player = and
    [ not $ _gettingHit flags
    , gunManagerCancelable $ _gunManager player
    , playerMovementSkillCancelable player
    , playerAttackCancelable player
    , not $ _onSpeedRail flags
    ]
    where flags = _flags player

playerWeaponMsgs :: Player -> AppEnv ThinkPlayerMsgsPhase [Msg ThinkPlayerMsgsPhase]
playerWeaponMsgs player = concat <$> traverse thinkWeapon (zip [0..] weapons)
    where
        thinkWeapon :: (Int, Some Weapon) -> AppEnv ThinkPlayerMsgsPhase [Msg ThinkPlayerMsgsPhase]
        thinkWeapon (i, Some wpn) = (W._think wpn) wpnThinkStatus player (_attack player) wpn
            where
                wpnAtkStatus
                    | canPlayerAttack player = WeaponAttackReady
                    | otherwise              = WeaponAttackNotReady
                wpnThinkStatus
                    | i == 0    = WeaponThinkForeground wpnAtkStatus
                    | otherwise = WeaponThinkBackground

        weapons = _weapons $ _weaponManager player

playerGunMsgs :: Player -> AppEnv ThinkPlayerMsgsPhase [Msg ThinkPlayerMsgsPhase]
playerGunMsgs player = thinkGunManager shootable player (_gunManager player)
    where
        flags    = _flags player
        canShoot = and
            [ not $ _gettingHit flags
            , playerMovementSkillCancelable player
            , playerAttackCancelable player
            , not $ _onSpeedRail flags
            ]

        shootable
            | canShoot  = Shootable
            | otherwise = NotShootable

playerMovementSkillMsgs :: Player -> AppEnv ThinkPlayerMsgsPhase [Msg ThinkPlayerMsgsPhase]
playerMovementSkillMsgs player = case _movementSkill player of
    Nothing                   -> return []
    Just (Some movementSkill) ->
        let
            flags       = _flags player
            canUseSkill = and
                [ not $ _gettingHit flags
                , gunManagerCancelable $ _gunManager player
                , playerAttackCancelable player
                , not $ _onSpeedRail flags
                ]
        in (MS._think movementSkill) canUseSkill player movementSkill

playerSecondarySkillMsgs :: Player -> AppEnv ThinkPlayerMsgsPhase [Msg ThinkPlayerMsgsPhase]
playerSecondarySkillMsgs player = thinkSecondarySkillManager canUseSkill player (_secondarySkillManager player)
    where
        flags       = _flags player
        canUseSkill = and
            [ not $ _gettingHit flags
            , gunManagerCancelable $ _gunManager player
            , playerMovementSkillCancelable player
            , playerAttackCancelable player
            , not $ _onSpeedRail flags
            ]

playerInvalidActionUiMsgs :: InputState -> Player -> [Msg ThinkPlayerMsgsPhase]
playerInvalidActionUiMsgs inputState player =
    let
        isPressed      = \alias input -> alias `aliasPressed` inputState || input `inPlayerInputBuffer` player
        isSkillPressed = \slot -> isSecondarySkillPressed inputState slot || isSecondarySkillPressedBuffer player slot

        mkMsgUiInvalidAction   = \alias -> mkMsg $ UiMsgInvalidAction alias
        mkMsgUiInvalidActionEx = \alias1 alias2 -> mkMsg $ UiMsgInvalidActionEx alias1 alias2
        mkMsgClearInputBuffer  = \input -> mkMsg $ PlayerMsgClearInputBuffer (S.singleton input)

        weapons           = _weapons $ _weaponManager player
        guns              = _guns $ _gunManager player
        secondarySkillMgr = _secondarySkillManager player
    in flip execState [] $ do
        when (isPressed WeaponAlias WeaponInput && null weapons) $
            modify ([mkMsgUiInvalidAction WeaponAlias, mkMsgClearInputBuffer WeaponInput] ++)
        when (isPressed SwitchWeaponAlias SwitchWeaponInput && length weapons < 2) $
            modify ([mkMsgUiInvalidAction SwitchWeaponAlias, mkMsgClearInputBuffer SwitchWeaponInput] ++)

        when (isPressed ShootAlias ShootInput && null guns) $
            modify ([mkMsgUiInvalidAction ShootAlias, mkMsgClearInputBuffer ShootInput] ++)
        when (isPressed SwitchGunAlias SwitchGunInput && length guns < 2) $
            modify ([mkMsgUiInvalidAction SwitchGunAlias, mkMsgClearInputBuffer SwitchGunInput] ++)

        when (isPressed MovementSkillAlias MovementSkillInput && isNothing (_movementSkill player)) $
            modify ([mkMsgUiInvalidAction MovementSkillAlias, mkMsgClearInputBuffer MovementSkillInput] ++)

        when (isSkillPressed SecondarySkillNeutralSlot && isNothing (_neutralSlot secondarySkillMgr)) $
            modify ([mkMsgUiInvalidAction SecondarySkillAlias, mkMsgClearInputBuffer SecondarySkillNeutralInput] ++)
        when (isSkillPressed SecondarySkillUpSlot && isNothing (_upSlot secondarySkillMgr)) $
            modify
                ( [mkMsgUiInvalidActionEx SecondarySkillAlias UpAlias, mkMsgClearInputBuffer SecondarySkillUpInput] ++
                )
        when (isSkillPressed SecondarySkillDownSlot && isNothing (_downSlot secondarySkillMgr)) $
            modify
                (
                    [ mkMsgUiInvalidActionEx SecondarySkillAlias DownAlias
                    , mkMsgClearInputBuffer SecondarySkillDownInput
                    ] ++
                )

        unlessM (gets null) $
            modify (mkMsg (AudioMsgPlaySoundCentered invalidActionSoundPath):)

readPlayerInItemInteractRange :: MsgsRead ThinkPlayerMsgsPhase m => m Bool
readPlayerInItemInteractRange = processMsgs <$> readMsgs
    where
        processMsgs :: [InfoMsgPayload] -> Bool
        processMsgs []     = False
        processMsgs (p:ps) = case p of
            InfoMsgPlayer playerInfo -> _inItemInteractRange playerInfo
            _                        -> processMsgs ps

playerTauntMsgs
    :: (ConfigsRead m, InputRead m, MsgsRead ThinkPlayerMsgsPhase m)
    => Player
    -> m [Msg ThinkPlayerMsgsPhase]
playerTauntMsgs player = do
    isTauntDisabled     <- readSettingsConfig _debug _disablePlayerTaunt
    inItemInteractRange <- readPlayerInItemInteractRange
    inputState          <- readInputState

    let
        onGround      = _touchingGround (_flags player :: PlayerFlags)
        isUpInput     = UpAlias `aliasHold` inputState
        isUpDownInput = isUpInput || DownAlias `aliasHold` inputState
        isTauntInput  =
            (isUpDownInput && InteractAlias `aliasPressed` inputState) || TauntInput `inPlayerInputBuffer` player
        tauntState   = _tauntState player

    flip execStateT [] $ do
        if
            | isTauntDisabled                        -> return ()
            | inItemInteractRange                    ->
                modify (mkMsg (PlayerMsgClearInputBuffer $ S.singleton TauntInput):)
            | otherwise                              -> when (onGround && canPlayerAttack player && isTauntInput) $
                modify (playerTauntStateStartTauntMsgs isUpInput tauntState ++)

        case playerAttackSprite player of
            Just atkSpr ->
                let
                    isFrameTag   = \t -> t `isSpriteFrameTag` atkSpr
                    frameChanged = _frameChanged atkSpr
                    frameIdx     = _frameIndex atkSpr
                in do
                    when (isFrameTag preTauntFrameTagName && frameChanged && frameIdx == 0) $
                        modify (playerTauntStateClearQueuedEnemyIdsMsg tauntState:)

                    when (isFrameTag preTauntFrameTagName) $ do
                        msg <- lift $ playerTauntStateUpdateEnemyIdsMsg tauntState
                        modify (msg:)

                    when (isFrameTag tauntActivateFrameTagName && frameChanged) $ do
                        msgs <- lift $ playerTauntStateActivateTauntMsgs tauntState
                        modify (msgs ++)

            _ -> return ()

thinkPlayer :: Player -> AppEnv ThinkPlayerMsgsPhase ()
thinkPlayer player
    | isPlayerInSpawnAnim player = return ()
    | isPlayerInDeathAnim player = return ()
    | otherwise                  = do
        inputState         <- readInputState
        moveSkillMsgs      <- playerMovementSkillMsgs player
        lockOnAimMsgs      <- thinkPlayerLockOnAim player (_lockOnAim player)
        tauntMsgs          <- playerTauntMsgs player
        secondarySkillMsgs <- playerSecondarySkillMsgs player
        gunMsgs            <- playerGunMsgs player
        weaponMsgs         <- playerWeaponMsgs player
        weaponMgrMsgs      <- thinkWeaponManager player (_weaponManager player)

        let
            attackMsgs          = maybe [] thinkAttack (_attack player)
            interactMsgs        = playerInteractMsgs inputState player
            invalidActionUiMsgs = playerInvalidActionUiMsgs inputState player

        writeMsgs . concat $
            [ moveSkillMsgs
            , lockOnAimMsgs
            , tauntMsgs
            , secondarySkillMsgs
            , gunMsgs
            , weaponMsgs
            , weaponMgrMsgs
            , attackMsgs
            , interactMsgs
            , invalidActionUiMsgs
            ]
