module Player.Gun.All.Revolver
    ( mkRevolverGun
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))

import Configs
import Configs.All.PlayerGun
import Configs.All.PlayerGun.Revolver
import Constants
import FileCache
import Msg
import Player
import Player.BufferedInputState
import Player.Gun
import Player.Gun.All.Revolver.Data
import Player.Gun.All.Revolver.Shot
import Player.Gun.All.Revolver.Util
import Player.Gun.FireDrawData
import Util
import Window.Graphics
import Window.InputState

revolverSoundPath          = "event:/SFX Events/Player/Guns/revolver"              :: FilePath
revolverAltSoundPath       = "event:/SFX Events/Player/Guns/revolver-alt"          :: FilePath
revolverAltChargeSoundPath = "event:/SFX Events/Player/Guns/revolver-alt-charge-c" :: FilePath
revolverReloadSoundPath    = "event:/SFX Events/Player/Guns/revolver-reload"       :: FilePath

decreaseCooldownMsgs :: [Msg ThinkPlayerMsgsPhase]
decreaseCooldownMsgs = [mkMsg $ PlayerMsgUpdateGun updateCooldown]
    where
        updateCooldown = \g ->
            let gData = _data g
            in g {_data = gData {_cooldown = _cooldown gData - timeStep}}

mkRevolverGun :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some Gun)
mkRevolverGun = do
    revolverData <- mkRevolverData

    return . Some $ (mkGun revolverData RevolverGun)
        { _fireDrawData = Just $ _shootFireDrawData revolverData
        , _think        = think
        , _update       = update
        }

continuousChargeSoundMsg :: RevolverData -> Pos2 -> Msg ThinkPlayerMsgsPhase
continuousChargeSoundMsg revolverData pos = mkMsg $ AudioMsgPlaySoundContinuous soundPath soundHashedId pos
    where
        soundPath     = revolverAltChargeSoundPath
        soundHashedId = _chargeSoundHashedId revolverData

thinkReadyStatus :: GunStatus -> InputState -> Player -> Gun RevolverData -> [Msg ThinkPlayerMsgsPhase]
thinkReadyStatus gunStatus inputState player revolver
    | isShootInput && canShoot && not canSpendMeter && bullets == maxBullets =
        [mkMsg $ UiMsgInsufficientMeter meterCost shootJustPressed]

    | isShootInput && canShoot && canSpendMeter =
        let
            (status', fireDrawData, cooldown', shotType, bullets')
                | shootDownPressed =
                    ( ContinuousShotsStatus
                    , _continuousChargeFireDrawData revolverData
                    , _continuousShotsChargeCd cfg
                    , RevolverContinuousShotType
                    , bullets
                    )
                | otherwise        =
                    ( NormalShotsStatus
                    , _shootFireDrawData revolverData
                    , _normalShotsCd cfg
                    , RevolverNormalShotType
                    , max 0 (bullets - 1)
                    )

            updateShoot = \g -> g
                { _data         = (_data g)
                    { _status   = status'
                    , _bullets  = bullets'
                    , _cooldown = cooldown'
                    }
                , _fireDrawData = Just fireDrawData
                }

            velMsgs
                | inAir && velY > 0.0 = [mkMsgEx (PlayerMsgSetVelocity $ _fireAirVel cfg) MsgAfterNormalOrder]
                | otherwise           = []
            meterMsgs
                | shootDownPressed    = []
                | otherwise           = [mkMsg $ PlayerMsgSpendMeter meterCost]
            projMsgs
                | shootDownPressed    = []
                | otherwise           = [mkMsg $ NewThinkProjectileMsgAddM (mkShotProjectile shotType player cfg)]
            soundMsgs
                | shootDownPressed    = [continuousChargeSoundMsg revolverData pos]
                | otherwise           = [mkMsg $ AudioMsgPlaySound revolverSoundPath pos]
        in velMsgs ++ meterMsgs ++ projMsgs ++ soundMsgs ++
            [ mkMsg $ PlayerMsgUpdateGun updateShoot
            , mkMsg PlayerMsgFiredGun
            ]

    | bullets < maxBullets && cooldown <= 0.0 =
        let
            updateReload = \g -> g
                { _data = (_data g)
                    { _bullets  = maxBullets
                    , _cooldown = 0.0
                    }
                }
        in
            [ mkMsg $ PlayerMsgUpdateGun updateReload
            , mkMsg $ AudioMsgPlaySound revolverReloadSoundPath pos
            ]

    | otherwise = decreaseCooldownMsgs

    where
        cfg           = _config (revolverData :: RevolverData)
        meterCost     = _shotMeterCost cfg
        canSpendMeter = canSpendPlayerMeter meterCost player
        canShoot      = gunStatus == ActiveStatus Shootable
        inAir         = not $ _touchingGround (_flags player)
        pos           = _pos (player :: Player)
        velY          = vecY $ _vel (player :: Player)
        revolverData  = _data revolver
        bullets       = _bullets revolverData
        cooldown      = _cooldown revolverData
        maxBullets    = _maxBullets cfg

        shootJustPressed  = ShootAlias `aliasPressed` inputState
        shootPressed      = shootJustPressed || ShootInput `inPlayerInputBuffer` player
        shootDownPressed  =
            (ShootAlias `aliasPressed` inputState && DownAlias `aliasHold` inputState) ||
            ShootDownInput `inPlayerInputBuffer` player
        shootHeld         = ShootAlias `aliasHold` inputState
        isNormalShotsHeld = shootHeld && bullets < _maxBullets cfg
        isShootInput      = shootPressed || shootDownPressed || isNormalShotsHeld

thinkNormalShotsStatus :: Player -> Gun RevolverData -> [Msg ThinkPlayerMsgsPhase]
thinkNormalShotsStatus player revolver
    | cooldown <= 0.0 =
        let
            pos                = _pos (player :: Player)
            bullets            = _bullets revolverData
            (status, cooldown')
                | bullets <= 0 = (ReloadStatus, _reloadCd cfg)
                | otherwise    = (ReadyStatus, _autoReloadCd cfg)

            soundMsgs
                | status == ReloadStatus = [mkMsg $ AudioMsgPlaySound revolverReloadSoundPath pos]
                | otherwise              = []

            updateReload = \g -> g
                { _data = (_data g)
                    { _status   = status
                    , _cooldown = cooldown'
                    }
                }
            updateGunMsg = mkMsg $ PlayerMsgUpdateGun updateReload
        in updateGunMsg:soundMsgs

    | otherwise = decreaseCooldownMsgs

    where
        revolverData = _data revolver
        cooldown     = _cooldown revolverData
        cfg          = _config (revolverData :: RevolverData)

thinkContinuousShotsStatus :: GunStatus -> InputState -> Player -> Gun RevolverData -> [Msg ThinkPlayerMsgsPhase]
thinkContinuousShotsStatus gunStatus inputState player revolver
    | isShootHeld && canShoot && canSpendMeter && cooldown <= 0.0 =
        let
            updateShoot = \g -> g
                { _data         = (_data g)
                    { _bullets  = max 0 (bullets - 1)
                    , _cooldown = _continuousShotsCd cfg
                    }
                , _fireDrawData = Just $ (_shootFireDrawData revolverData :: GunFireDrawData)
                    { _muzzleFlash = Just $ _continuousMuzzleFlash revolverData
                    }
                }

            velMsgs
                | inAir && velY > 0.0 = [mkMsgEx (PlayerMsgSetVelocity $ _fireAirVel cfg) MsgAfterNormalOrder]
                | otherwise           = []
        in velMsgs ++
            [ mkMsg $ PlayerMsgUpdateGun updateShoot
            , mkMsg PlayerMsgFiredGun
            , mkMsg $ NewThinkProjectileMsgAddM (mkShotProjectile RevolverContinuousShotType player cfg)
            , mkMsg $ AudioMsgPlaySound revolverAltSoundPath pos
            , continuousChargeSoundMsg revolverData pos
            , mkMsg $ PlayerMsgSpendMeter meterCost
            ]

    | not isShootHeld && bullets >= maxBullets && isContinuousChargeMinHeld =
        let
            cancelShoot = \g -> g
                { _data         = (_data g)
                    { _status   = ReadyStatus
                    , _cooldown = 0.0
                    }
                , _fireDrawData = Just $ _continuousCancelFireDrawData revolverData
                }
        in
            [ mkMsg $ PlayerMsgUpdateGun cancelShoot
            , mkMsg PlayerMsgFiredGun
            ]

    | bullets < maxBullets && cooldown <= 0.0 =
        let
            updateReload = \g -> g
                { _data = (_data g)
                    { _status   = ReadyStatus
                    , _bullets  = maxBullets
                    , _cooldown = 0.0
                    }
                }
        in
            [ mkMsg $ PlayerMsgUpdateGun updateReload
            , mkMsg $ AudioMsgPlaySound revolverReloadSoundPath pos
            ]

    | otherwise = continuousChargeSoundMsg revolverData pos:decreaseCooldownMsgs

    where
        cfg                       = _config (revolverData :: RevolverData)
        meterCost                 = _shotMeterCost cfg
        canSpendMeter             = canSpendPlayerMeter meterCost player
        canShoot                  = gunStatus == ActiveStatus Shootable
        inAir                     = not $ _touchingGround (_flags player)
        pos                       = _pos (player :: Player)
        velY                      = vecY $ _vel (player :: Player)
        revolverData              = _data revolver
        bullets                   = _bullets revolverData
        cooldown                  = _cooldown revolverData
        maxBullets                = _maxBullets cfg
        isShootHeld               = ShootAlias `aliasHold` inputState
        isContinuousChargeMinHeld = _continuousShotsChargeCd cfg - cooldown >= _continuousChargeMinSecs cfg

thinkReloadStatus :: InputState -> Gun RevolverData -> [Msg ThinkPlayerMsgsPhase]
thinkReloadStatus inputState revolver
    | cooldown <= 0.0 && not shootHeld =
        let
            updateReady = \g -> g
                { _data = (_data g)
                    { _status   = ReadyStatus
                    , _bullets  = _maxBullets cfg
                    , _cooldown = 0.0
                    }
                }
        in [mkMsg $ PlayerMsgUpdateGun updateReady]

    | otherwise = decreaseCooldownMsgs

    where
        shootHeld    = ShootAlias `aliasHold` inputState
        revolverData = _data revolver
        cooldown     = _cooldown revolverData
        cfg          = _config (revolverData :: RevolverData)

think :: InputRead m => GunThink RevolverData m
think gunStatus player revolver = do
    inputState <- readInputState

    return $ case _status (_data revolver :: RevolverData) of
        ReadyStatus           -> thinkReadyStatus gunStatus inputState player revolver
        NormalShotsStatus     -> thinkNormalShotsStatus player revolver
        ContinuousShotsStatus -> thinkContinuousShotsStatus gunStatus inputState player revolver
        ReloadStatus          -> thinkReloadStatus inputState revolver

update :: ConfigsRead m => GunUpdate RevolverData m
update revolver = do
    cfg <- readConfig _playerGun _revolver
    let
        fireDrawData = _fireDrawData revolver <&> \fdd ->
            fdd {_calculatePlayerAimBody = calculatePlayerAimBody cfg}

    return $ revolver
        { _data         = (_data revolver) {_config = cfg} :: RevolverData
        , _fireDrawData = fireDrawData
        }
