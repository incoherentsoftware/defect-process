module Player.Weapon.All.Sword
    ( mkSwordWeapon
    ) where

import Control.Applicative    ((<|>))
import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))
import Data.Maybe             (fromMaybe, isJust, listToMaybe)
import System.FilePath        (takeBaseName)
import qualified Data.Map as M

import Attack
import Attack.Hit
import Attack.Projectile
import Collision.Hitbox
import Configs
import Configs.All.Player
import Configs.All.PlayerWeapon
import Configs.All.PlayerWeapon.Sword
import Constants
import FileCache
import Id
import Msg
import Player
import Player.BufferedInputState
import Player.Weapon
import Player.Weapon.All.Sword.AttackOrb
import Player.Weapon.All.Sword.Data
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

releaseFrameTagName      = FrameTagName "release"         :: FrameTagName
summonFrameTagName       = FrameTagName "summon"          :: FrameTagName
slash3AoeFrameTagName    = FrameTagName "slash3Aoe"       :: FrameTagName
slash2HeavyFrameTagName  = FrameTagName "slash2Heavy"     :: FrameTagName
airDownSlashFrameTagName = FrameTagName "airDownSlash"    :: FrameTagName
noFallSlashLandFrameTag  = FrameTagName "noFallSlashLand" :: FrameTagName

chargeOverlaySoundPath = "event:/SFX Events/Player/Weapons/Sword/charge-overlay-c" :: FilePath

mkSwordWeapon :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some Weapon)
mkSwordWeapon = do
    swordData <- mkSwordData
    return . Some $ (mkWeapon swordData SwordWeapon)
        { _think       = thinkSword
        , _update      = updateSword
        , _drawOverlay = drawSwordChargeOverlay
        }

thinkSword :: (InputRead m, MonadIO m, MsgsWrite ThinkPlayerMsgsPhase m) => WeaponThink SwordData m
thinkSword weaponThinkStatus player currentAtk sword = do
    let swordData = _data sword

    atkFromInput <- case weaponThinkStatus of
        WeaponThinkForeground WeaponAttackReady -> mkAttackFromInput swordData player currentAtk
        _                                       -> return Nothing

    newAtk <- if
        | isJust atkFromInput -> return atkFromInput
        | otherwise           -> maybe (return Nothing) (alterSwordAttack swordData player) currentAtk

    let
        setAtkMsgs = maybe [] (pure . mkMsg . PlayerMsgSetAttack) newAtk
        atk        = newAtk <|> currentAtk
        thinkMsgs  = thinkSwordAttack swordData atk player
    chargeMsgs <- thinkSwordCharge swordData weaponThinkStatus player atk

    return $ setAtkMsgs ++ thinkMsgs ++ chargeMsgs

thinkSwordCharge
    :: InputRead m
    => SwordData
    -> WeaponThinkStatus
    -> Player
    -> Maybe Attack
    -> m [Msg ThinkPlayerMsgsPhase]
thinkSwordCharge swordData wpnThinkStatus player atk = (chargeAudioMsgs ++) <$> chargeInputMsgs
    where
        chargeStatus    = _chargeStatus swordData
        hashedId        = _chargeSoundHashedId swordData
        pos             = _pos (player :: Player)
        chargeAudioMsgs = case chargeStatus of
            SwordNoChargeStatus        -> []
            SwordPartialChargeStatus _ -> []
            SwordFullChargeStatus      -> case wpnThinkStatus of
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
                SwordNoChargeStatus        -> return []
                SwordPartialChargeStatus _ -> return []
                SwordFullChargeStatus      -> readInputState <&> \inputState ->
                    let
                        cfg                   = _config (swordData :: SwordData)
                        chargeMeterCost       = _chargeMeterCost cfg
                        weaponNotHeld         = not $ WeaponAlias `aliasHold` inputState
                        wpnAtkDescs           = _attackDescriptions swordData
                        chargeReleaseAtkDescs = [_chargeRelease wpnAtkDescs, _airChargeRelease wpnAtkDescs]
                        isChargeReleaseAtk    = maybe False (`attackIn` chargeReleaseAtkDescs) atk
                    in if
                        | _prevChargeStatus swordData /= SwordFullChargeStatus ->
                            [mkMsg (PlayerMsgSpendMeter chargeMeterCost)]
                        | weaponNotHeld && not isChargeReleaseAtk              ->
                            -- refund meter if released button during uncancelable action and there's no attack
                            [mkMsg $ PlayerMsgGainMeter NullId chargeMeterCost]
                        | otherwise                                            -> []

thinkSwordAttack :: SwordData -> Maybe Attack -> Player -> [Msg ThinkPlayerMsgsPhase]
thinkSwordAttack _ Nothing _                 = []
thinkSwordAttack swordData (Just atk) player
    | atk `attackIn` [chargeRelease, airChargeRelease] && isAtkChargeReleaseFrame && atkFrameChanged =
        let
            pos'                = offsetPos . _chargeOverlaySprOffset $ _config (player :: Player)
            mkChargeReleaseProj = mkPlayerAttackProjectile pos' dir chargeReleaseProjAtkDesc
        in [mkMsg $ NewThinkProjectileMsgAddM mkChargeReleaseProj]

    | atk `attackIs` summonAtkOrb && isSummonFrame && atkFrameChanged =
        let
            pos'         = offsetPos . _summonAttackOrbOffset $ _config (swordData :: SwordData)
            mkAttackOrb' = mkAttackOrb pos' dir swordData
        in [mkMsg $ EnemyMsgAddM mkAttackOrb']

    | otherwise = []

    where
        pos = _pos (player :: Player)
        dir = _dir (player :: Player)

        offsetPos = \offset -> pos `vecAdd` (offset `vecFlip` dir)

        atkFrameChanged         = attackFrameChanged atk
        isAtkChargeReleaseFrame = releaseFrameTagName `isAttackFrameTag` atk
        isSummonFrame           = summonFrameTagName `isAttackFrameTag` atk

        wpnAtkDescs              = _attackDescriptions swordData
        chargeRelease            = _chargeRelease wpnAtkDescs
        airChargeRelease         = _airChargeRelease wpnAtkDescs
        chargeReleaseProjAtkDesc = _chargeReleaseProjectile wpnAtkDescs
        summonAtkOrb             = _summonAttackOrb wpnAtkDescs

mkAttackFromInput
    :: (InputRead m, MonadIO m, MsgsWrite ThinkPlayerMsgsPhase m)
    => SwordData
    -> Player
    -> Maybe Attack
    -> m (Maybe Attack)
mkAttackFromInput swordData player attack = fromInput =<< readInputState
    where
        pos          = _pos (player :: Player)
        wpnAtkDescs  = _attackDescriptions swordData
        summonAtkOrb = _summonAttackOrb wpnAtkDescs
        cfg          = _config (swordData :: SwordData)

        mkSummonAtkOrbAttack :: (MonadIO m1, MsgsWrite ThinkPlayerMsgsPhase m1) => Direction -> m1 (Maybe Attack)
        mkSummonAtkOrbAttack atkDir
            | canSpendMeter = do
                writeMsgs [mkMsg $ PlayerMsgSpendMeter summonAtkOrbCost]
                Just <$> mkAttack pos atkDir summonAtkOrb
            | otherwise     = do
                writeMsgs [mkMsg $ UiMsgInsufficientMeter summonAtkOrbCost False]
                return Nothing
            where
                summonAtkOrbCost = _summonAttackOrbCost cfg
                canSpendMeter    = canSpendPlayerMeter summonAtkOrbCost player

        mkSlashAoeAttack :: MonadIO m1 => Direction -> m1 (Maybe Attack)
        mkSlashAoeAttack atkDir = do
            onHitId <- newId
            let
                -- make attack always knock enemy away from player
                slash3Aoe = (_slash3Aoe wpnAtkDescs)
                    { _onHitType = AddedOnHitType $ \enHbx enId atk ->
                        let
                            atkPos          = _pos (atk :: Attack)
                            atkHitbox       = fromMaybe (dummyHitbox atkPos) (attackHitbox atk)
                            atkIntersectPos = hitboxAvgIntersectPos atkHitbox enHbx
                            neg             = if
                                | vecX (hitboxCenter enHbx) < vecX pos -> -1
                                | otherwise                            -> 1
                            slash3AoeHitVel = _slash3AoeHitVel cfg
                            atkVelX         = vecX slash3AoeHitVel * neg
                            atkVelY         = vecY slash3AoeHitVel
                            atkHit          = (mkAttackHitEx atkIntersectPos atk)
                                { _hashedId       = hashId onHitId
                                , _vel            = Vel2 atkVelX atkVelY
                                , _damage         = Damage 0
                                , _stagger        = Stagger 0
                                , _alwaysLaunches = True
                                }
                        in [mkMsgToEx (HurtMsgAttackHit atkHit) enId MsgEndOrder]
                    }
            Just <$> mkAttack pos atkDir slash3Aoe

        fromInput :: (MonadIO m1, MsgsWrite ThinkPlayerMsgsPhase m1) => InputState -> m1 (Maybe Attack)
        fromInput inputState
            -- summon attack orb (left)
            | (inPlayerTapInputBuffer [LeftInput, LeftInput] player || isPlayerInputBufferQCF LeftDir player) &&
              not rightHeld && weaponPressed && onGround && attackIsNot summonAtkOrb = mkSummonAtkOrbAttack LeftDir

            -- summon attack orb (right)
            | (inPlayerTapInputBuffer [RightInput, RightInput] player || isPlayerInputBufferQCF RightDir player) &&
              not leftHeld && weaponPressed && onGround && attackIsNot summonAtkOrb = mkSummonAtkOrbAttack RightDir

            -- flurry stab
            | onGround && weaponDownPressed && attackIsNot flurryStab = mkAttack' flurryStab

            -- falling slash
            | inAir && weaponDownPressed && attackIsNot fallSlash = mkAttack' fallSlash

            -- up slash
            | onGround && weaponUpPressed && attackIsNot upSlash = mkAttack' upSlash

            -- flurry thrust
            | weaponPressed && attackIs' flurryStab && atkCancelable = mkAttack' flurryThrust

            | inAir && weaponPressed = if
                -- air down slash
                | attackIn' [airSlash2, airSlash1] && isAirDownSlashFrameTag -> mkAttack' airDownSlash
                -- air slash 3
                | attackIs' airSlash2 && atkCancelable                       -> mkAttack' airSlash3
                -- air slash 2
                | attackIs' airSlash1 && atkCancelable                       -> mkAttack' airSlash2
                -- air slash 1
                | otherwise                                                  -> if
                    | attackIsNot airSlash3 -> mkAttack' airSlash1
                    | otherwise             -> return Nothing

            | onGround && weaponPressed = if
                -- slash3 aoe
                | attackIn' [slash2, slash2Heavy] && isSlash3AoeFrameTag -> mkSlashAoeAttack dir

                -- slash3
                | attackIn' [slash2, slash2Heavy] && atkCancelable -> mkAttack' slash3

                -- slash2-heavy
                | attackIs' slash1 && isSlash2HeavyFrameTag -> mkAttack' slash2Heavy

                -- slash2
                | attackIs' slash1 && atkCancelable -> mkAttack' slash2

                -- slash1
                | otherwise -> if
                    | attackNotIn [slash3, slash3Aoe] -> mkAttack' slash1
                    | otherwise                       -> return Nothing

            -- charge release
            | weaponNotHeld && _chargeStatus swordData == SwordFullChargeStatus =
                let chargeReleaseF = if inAir then _airChargeRelease else _chargeRelease
                in mkAttack' $ chargeReleaseF wpnAtkDescs

            | otherwise = return Nothing

            where
                leftHeld        = LeftAlias `aliasHold` inputState
                rightHeld       = RightAlias `aliasHold` inputState
                dir
                    | leftHeld  = LeftDir
                    | rightHeld = RightDir
                    | otherwise = _dir (player :: Player)

                attackIs'   = \atkDesc -> maybe False (`attackIs` atkDesc) attack
                attackIsNot = \atkDesc -> not $ attackIs' atkDesc
                attackIn'   = \atkDescs -> maybe False (`attackIn` atkDescs) attack
                attackNotIn = \atkDescs -> and $ map (not . attackIs') atkDescs
                mkAttack'   = \atkDesc -> Just <$> mkAttack pos dir atkDesc

                weaponPressed     = WeaponAlias `aliasPressed` inputState || WeaponInput `inPlayerInputBuffer` player
                weaponUpPressed   =
                    (WeaponAlias `aliasPressed` inputState && UpAlias `aliasHold` inputState) ||
                    WeaponUpInput `inPlayerInputBuffer` player
                weaponDownPressed =
                    (WeaponAlias `aliasPressed` inputState && DownAlias `aliasHold` inputState) ||
                    WeaponDownInput `inPlayerInputBuffer` player
                weaponNotHeld     = not $ WeaponAlias `aliasHold` inputState

                onGround               = _touchingGround $ _flags player
                inAir                  = not onGround
                atkCancelable          = fromMaybe True (attackCancelable <$> attack)
                isSlash3AoeFrameTag    = maybe False (slash3AoeFrameTagName `isAttackFrameTag`) attack
                isSlash2HeavyFrameTag  = maybe False (slash2HeavyFrameTagName `isAttackFrameTag`) attack
                isAirDownSlashFrameTag = maybe False (airDownSlashFrameTagName `isAttackFrameTag`) attack

                slash1       = _slash1 wpnAtkDescs
                slash2       = _slash2 wpnAtkDescs
                slash2Heavy  = _slash2Heavy wpnAtkDescs
                slash3       = _slash3 wpnAtkDescs
                slash3Aoe    = _slash3Aoe wpnAtkDescs
                fallSlash    = _fallSlash wpnAtkDescs
                upSlash      = _upSlash wpnAtkDescs
                airSlash1    = _airSlash1 wpnAtkDescs
                airSlash2    = _airSlash2 wpnAtkDescs
                airDownSlash = _airDownSlash wpnAtkDescs
                airSlash3    = _airSlash3 wpnAtkDescs
                flurryStab   = _flurryStab wpnAtkDescs
                flurryThrust = _flurryThrust wpnAtkDescs

alterSwordAttack :: MonadIO m => SwordData -> Player -> Attack -> m (Maybe Attack)
alterSwordAttack swordData player atk
    -- falling slash -> falling slash land
    | attackIs' _fallSlash && onGround && not (noFallSlashLandFrameTag `isAttackFrameTag` atk) =
        let
            pos = _pos (player :: Player)
            dir = _dir (player :: Player)
        in Just <$> mkAttack pos dir (_fallSlashLand wpnAtkDescs)

    -- air slash1/2/3, air charge-release land
    | attackIn' [_airSlash1, _airSlash2, _airSlash3, _airChargeRelease] && onGround = return finishAtk

    -- cancel grounded attacks if now in air
    | not onGround && attackIn' groundedAtkDescs = return finishAtk

    | otherwise = return Nothing

    where
        wpnAtkDescs = _attackDescriptions swordData
        onGround    = _touchingGround $ _flags player

        attackIs' = \atkDescF -> attackIs atk (atkDescF wpnAtkDescs)
        attackIn' = \atkDescFs -> attackIn atk [atkDescF wpnAtkDescs | atkDescF <- atkDescFs]
        finishAtk = Just $ finishAttack atk

        groundedAtkDescs =
            [ _slash1
            , _slash2
            , _slash2Heavy
            , _slash3
            , _slash3Aoe
            , _upSlash
            , _flurryStab
            , _flurryThrust
            , _chargeRelease
            , _summonAttackOrb
            , _fallSlashLand
            ]

updateSword :: (ConfigsRead m, InputRead m, MsgsWrite UpdatePlayerMsgsPhase m) => WeaponUpdate SwordData m
updateSword WeaponUpdateBackground _ _ sword      = return sword
updateSword WeaponUpdateForeground player _ sword = do
    inputState <- readInputState

    let
        swordData               = _data sword
        cfg                     = _config (swordData :: SwordData)
        chargeHeldThresholdSecs = _chargeHeldThresholdSecs cfg

        weaponHeld      = WeaponAlias `aliasHold` inputState
        chargeMeterCost = _chargeMeterCost cfg
        canSpendMeter   = canSpendPlayerMeter chargeMeterCost player

        chargeStatus  = _chargeStatus swordData
        chargeStatus' = case chargeStatus of
            SwordNoChargeStatus
                | weaponHeld -> SwordPartialChargeStatus timeStep
            SwordPartialChargeStatus chargeSecs
                | weaponHeld ->
                    let chargeSecs' = chargeSecs + timeStep
                    in if
                        | chargeSecs' >= chargeHeldThresholdSecs && canSpendMeter -> SwordFullChargeStatus
                        | otherwise                                               ->
                            SwordPartialChargeStatus chargeSecs'
            SwordFullChargeStatus
                | weaponHeld -> SwordFullChargeStatus
            _                -> SwordNoChargeStatus

    let chargeAboveThreshold = swordChargeStatusChargeSecs chargeStatus' >= chargeHeldThresholdSecs
    when (chargeAboveThreshold && chargeStatus' /= SwordFullChargeStatus && not canSpendMeter) $
        writeMsgs [mkMsg $ UiMsgInsufficientMeter chargeMeterCost False]

    swordData' <- readConfig _playerWeapon _sword <&> \cfg' -> swordData
        { _prevChargeStatus = chargeStatus
        , _chargeStatus     = chargeStatus'
        , _chargeOverlaySpr = updateSprite $ _chargeOverlaySpr swordData
        , _config           = cfg'
        }
    return $ sword {_data = swordData'}

drawSwordChargeOverlay :: (GraphicsReadWrite m, MonadIO m) => WeaponDrawOverlay SwordData m
drawSwordChargeOverlay WeaponDrawOverlayBackground _ _ _          = return ()
drawSwordChargeOverlay WeaponDrawOverlayForeground player _ sword =
    when (_chargeStatus swordData == SwordFullChargeStatus) $
        drawSprite pos' dir playerWeaponOverlayZIndex chargeOverlaySpr
    where
        swordData        = _data sword
        dir              = _dir (player :: Player)
        pos              = _pos (player :: Player)
        chargeOverlaySpr = _chargeOverlaySpr swordData
        playerCfg        = _config (player :: Player)

        offset = fromMaybe (_chargeOverlaySprOffset playerCfg) $ do
            spr            <-
                playerAttackSprite player <|> playerMovementSkillSprite player <|> Just (_sprite (player :: Player))
            let sprFileName = takeBaseName $ _filePath (spr :: Sprite)
            offsets        <- M.lookup sprFileName (_chargeOverlaySprOffsetMap playerCfg)
            listToMaybe (drop (_int (_frameIndex spr :: FrameIndex)) offsets) <|> maybeLast offsets

        pos' = pos `vecAdd` (offset `vecFlip` dir)
