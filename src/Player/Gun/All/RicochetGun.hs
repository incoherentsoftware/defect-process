module Player.Gun.All.RicochetGun
    ( mkRicochetGun
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (for_)

import Configs
import Configs.All.PlayerGun
import Configs.All.PlayerGun.RicochetGun
import Constants
import FileCache
import Msg
import Player
import Player.BufferedInputState
import Player.Gun
import Player.Gun.All.RicochetGun.Data
import Player.Gun.All.RicochetGun.Shot
import Player.Gun.FireDrawState
import Player.Meter
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

ricochetGunSoundPath              = "event:/SFX Events/Player/Guns/ricochet-gun"                 :: FilePath
ricochetGunAddBounceSoundPath     = "event:/SFX Events/Player/Guns/ricochet-gun-add-bounce"      :: FilePath
ricochetGunAddBounceFullSoundPath = "event:/SFX Events/Player/Guns/ricochet-gun-add-bounce-full" :: FilePath

notSelfCancelableFrameTagName = FrameTagName "notSelfCancelable" :: FrameTagName
allowShootHoldFrameTagName    = FrameTagName "allowShootHold"    :: FrameTagName
addedFrameTagName             = FrameTagName "added"             :: FrameTagName

numBouncesPowerLevelMultiplier = 2 :: Int

mkRicochetGun :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some Gun)
mkRicochetGun = do
    ricochetGunData <- mkRicochetGunData

    return . Some $ (mkGun ricochetGunData RicochetGun)
        { _fireDrawData = Just $ _shootFireDrawData ricochetGunData
        , _think        = think
        , _update       = update
        , _drawOverlay  = drawOverlay
        }

fireShot :: GunStatus -> InputState -> Player -> Gun RicochetGunData -> [Msg ThinkPlayerMsgsPhase]
fireShot gunStatus inputState player ricochetGun
    | isPowerUpInput && canPowerUp = if
        -- power up
        |  canSpendPowerUpMeter && powerLevel < _maxPowerLevel cfg -> if
            | isPowerUpNotSelfCancelable -> []
            | otherwise                  ->
                let
                    updatePowerUp = \g ->
                        let gData = _data g
                        in g
                            { _data         = gData
                                { _showUiOverlayTtl   = _showUiOverlaySecs cfg
                                , _isPowerUpShootHeld = True
                                }
                            , _fireDrawData = Just $ _addBounceFireDrawData gData
                            }
                in
                    [ mkMsg $ PlayerMsgUpdateGun updatePowerUp
                    , mkMsg PlayerMsgFiredGun
                    , mkMsg $ AudioMsgPlaySound ricochetGunAddBounceSoundPath pos
                    ] ++ shootVelMsgs

        -- power up full
        | shootPressed && powerLevel >= _maxPowerLevel cfg ->
            let
                updateShowUiOverlay = \g -> g
                    { _data = (_data g) {_showUiOverlayTtl = _showUiOverlaySecs cfg}
                    }
                audioMsgs           = if
                    | _showUiOverlayTtl ricochetGunData <= 0.0 || shootJustPressed ->
                        [mkMsg $ AudioMsgPlaySound ricochetGunAddBounceFullSoundPath pos]
                    | otherwise                                                    -> []
            in mkMsg (PlayerMsgUpdateGun updateShowUiOverlay):audioMsgs

        -- insufficient meter
        | shootPressed ->
            let
                updateShowUiOverlay = \g -> g
                    { _data = (_data g) {_showUiOverlayTtl = _showUiOverlaySecs cfg}
                    }
            in
                [ mkMsg $ PlayerMsgUpdateGun updateShowUiOverlay
                , mkMsg $ UiMsgInsufficientMeter powerUpMeterCost shootJustPressed
                ]

        | otherwise -> []

    | shootPressed && canShoot && not canSpendShootMeter =
        [mkMsg $ UiMsgInsufficientMeter shootMeterCost shootJustPressed]

    -- shoot
    | shootPressed && canShoot && canSpendShootMeter =
        let
            updateShoot = \g -> g
                { _data         = (_data g)
                    { _status           = BetweenShotsStatus
                    , _powerLevel       = 0
                    , _cooldownTtl      = _reloadCd cfg
                    , _showUiOverlayTtl = _showUiOverlaySecs cfg
                    }
                , _fireDrawData = Just $ _shootFireDrawData (_data g)
                }

            numBounces = _powerLevel ricochetGunData * numBouncesPowerLevelMultiplier
        in
            [ mkMsg $ PlayerMsgUpdateGun updateShoot
            , mkMsg PlayerMsgFiredGun
            , mkMsg $ NewThinkProjectileMsgAddM (mkShotProjectile player numBounces)
            , mkMsg $ AudioMsgPlaySound ricochetGunSoundPath pos
            , mkMsg $ PlayerMsgSpendMeter shootMeterCost
            ] ++ shootVelMsgs

    | otherwise = []

    where
        ricochetGunData      = _data ricochetGun
        powerLevel           = _powerLevel ricochetGunData
        cfg                  = _config (ricochetGunData :: RicochetGunData)
        powerUpMeterCost     = _powerUpMeterCost cfg
        canSpendPowerUpMeter = canSpendPlayerMeter powerUpMeterCost player

        shootMeterCost
            | powerLevel > 0 = MeterValue 0
            | otherwise      = _shotMeterCost cfg
        canSpendShootMeter = canSpendPlayerMeter shootMeterCost player

        canPowerUp = gunStatus == ActiveStatus Shootable
        canShoot   = _status ricochetGunData == ReadyStatus && canPowerUp
        inAir      = not $ _touchingGround (_flags player)
        pos        = _pos (player :: Player)
        velY       = vecY $ _vel (player :: Player)

        shootJustPressed = ShootAlias `aliasPressed` inputState
        shootPressed     = shootJustPressed || ShootInput `inPlayerInputBuffer` player

        isPowerUpInput =
            let
                isAllowedHoldInput = case _torso <$> playerGunFireDrawSprites player of
                    Just torsoSpr
                        | allowShootHoldFrameTagName `isSpriteFrameTag` torsoSpr -> _isPowerUpShootHeld ricochetGunData
                    _                                                            -> False
            in (shootPressed && DownAlias `aliasHold` inputState) || isAllowedHoldInput

        isPowerUpNotSelfCancelable = case _torso <$> playerGunFireDrawSprites player of
            Just torsoSpr
                | notSelfCancelableFrameTagName `isSpriteFrameTag` torsoSpr -> True
            _                                                               -> False

        shootVelMsgs
            | inAir && velY > 0.0 = [mkMsgEx (PlayerMsgSetVelocity $ _fireAirVel cfg) MsgAfterNormalOrder]
            | otherwise           = []

decreaseCooldownsMsg :: Msg ThinkPlayerMsgsPhase
decreaseCooldownsMsg = mkMsg $ PlayerMsgUpdateGun updateCooldownStatus
    where
        updateCooldownStatus = \g ->
            let
                gData            = _data g
                cooldownTtl      = max 0.0 (_cooldownTtl gData - timeStep)
                status           = if cooldownTtl <= 0.0 then ReadyStatus else BetweenShotsStatus
                showUiOverlayTtl = max 0.0 (_showUiOverlayTtl gData - timeStep)
            in g
                { _data = gData
                    { _status           = status
                    , _cooldownTtl      = cooldownTtl
                    , _showUiOverlayTtl = showUiOverlayTtl
                    }
                }

think :: (ConfigsRead m, InputRead m) => GunThink RicochetGunData m
think gunStatus player ricochetGun =
    let
        ricochetGunData  = _data ricochetGun
        cfg              = _config (ricochetGunData :: RicochetGunData)
        maxPowerLevel    = _maxPowerLevel cfg
        powerUpMeterCost = _powerUpMeterCost cfg

        powerUpMsgs = case _torso <$> playerGunFireDrawSprites player of
            Just torsoSpr
                | addedFrameTagName `isSpriteFrameTag` torsoSpr && _frameChanged torsoSpr ->
                    let
                        updatePowerLevel = \g ->
                            let gData = _data g
                            in g
                                { _data = gData
                                    { _powerLevel  = min maxPowerLevel (_powerLevel gData + 1)
                                    }
                                }
                    in
                        [ mkMsg $ PlayerMsgUpdateGun updatePowerLevel
                        , mkMsg $ PlayerMsgSpendMeter powerUpMeterCost
                        ]

            _ -> []
        in do
            inputState <- readInputState
            return $ decreaseCooldownsMsg:powerUpMsgs ++ fireShot gunStatus inputState player ricochetGun

update :: (ConfigsRead m, InputRead m) => GunUpdate RicochetGunData m
update ricochetGun = do
    inputState <- readInputState
    let
        isPowerUpShootHeld
            | ShootAlias `aliasHold` inputState = _isPowerUpShootHeld $ _data ricochetGun
            |otherwise                          = False

    cfg <- readConfig _playerGun _ricochetGun

    return $ ricochetGun
        { _data = (_data ricochetGun)
            { _isPowerUpShootHeld = isPowerUpShootHeld
            , _config             = cfg
            }
        }

drawOverlay :: (GraphicsReadWrite m, MonadIO m) => GunDrawOverlay RicochetGunData m
drawOverlay GunDrawOverlayBackground _ _                = return ()
drawOverlay GunDrawOverlayForeground player ricochetGun = do
    playerPos           <- (_pos (player :: Player) `vecAdd`) <$> playerLerpOffset player
    let ricochetGunData  = _data ricochetGun

    when (_showUiOverlayTtl ricochetGunData > 0.0) $
        let
            cfg        = _config (ricochetGunData :: RicochetGunData)
            overlayPos = playerPos `vecAdd` _uiOverlayOffset cfg
        in do
            for_ [1.._maxPowerLevel cfg] $ \i ->
                let
                    offsetX = _uiSegmentOffsetX cfg * fromIntegral i
                    pos     = overlayPos `vecAdd` Pos2 offsetX 0.0
                in drawImage pos RightDir playerGunOverlayZIndex (_uiSegmentBackdropImage ricochetGunData)

            for_ [1.._powerLevel ricochetGunData] $ \i ->
                let
                    offsetX = _uiSegmentOffsetX cfg * fromIntegral i
                    pos     = overlayPos `vecAdd` Pos2 offsetX 0.0
                in drawImage pos RightDir playerGunOverlayZIndex (_uiSegmentFilledImage ricochetGunData)
