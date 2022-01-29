module Player.Gun.All.Revolver
    ( mkRevolverGun
    , calculatePlayerAimBody
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Vector as V

import Configs
import Configs.All.PlayerGun
import Configs.All.PlayerGun.Revolver
import FileCache
import Msg
import Player
import Player.AimBody
import Player.BufferedInputState
import Player.Gun
import Player.Gun.All.Revolver.Data
import Player.Gun.All.Revolver.Shot
import Player.Gun.FireDrawAngle
import Player.Gun.FireDrawData
import Player.Gun.FireDrawState.LegsState
import Player.Gun.MuzzleFlash
import Util
import Window.Graphics
import Window.InputState

revolverSoundPath       = "event:/SFX Events/Player/Guns/revolver"        :: FilePath
revolverReloadSoundPath = "event:/SFX Events/Player/Guns/revolver-reload" :: FilePath

muzzleFlashSpriteFileNames = NE.fromList
    [ "revolver-muzzle-flash-a.spr"
    , "revolver-muzzle-flash-b.spr"
    , "revolver-muzzle-flash-c.spr"
    , "revolver-muzzle-flash-d.spr"
    ] :: NE.NonEmpty FileName

loadRevolverSprite :: (FileCache m, GraphicsRead m, MonadIO m) => FileName -> m Sprite
loadRevolverSprite fileName = loadPackSprite $ PackResourceFilePath "data/player/player-guns.pack" fileName

decreaseCooldownMsgs :: [Msg ThinkPlayerMsgsPhase]
decreaseCooldownMsgs = [mkMsg $ PlayerMsgUpdateGun updateCooldown]
    where updateCooldown = \g -> g {_data = decreaseRevolverDataCooldown (_data g)}

calculatePlayerAimBody
    :: RevolverConfig
    -> GunFireDrawAngle
    -> Pos2
    -> Radians
    -> Direction
    -> LegsState
    -> PlayerAimBody
calculatePlayerAimBody cfg fireDrawAngle playerPos angle playerDir legsState = PlayerAimBody
    { _headAngle       = headAngle
    , _neckPos         = neckPos
    , _torsoAngle      = torsoAngle
    , _leadShoulderPos = leadShoulderPos
    , _rearShoulderPos = rearShoulderPos
    , _hipsPos         = hipsPos
    , _leadArmAngle    = leadArmAngle
    , _rearArmAngle    = rearArmAngle
    , _aimDir          = calculateAimAngleDir angle
    }
    where
        torsoNeckOffset        = fromMaybe zeroPos2 (fireDrawAngle `M.lookup` _torsoNeckOffsets cfg)
        rearShoulderHipsOffset = fromMaybe zeroPos2 (fireDrawAngle `M.lookup` _rearShoulderHipsOffsets cfg)
        leadShoulderHipsOffset = fromMaybe zeroPos2 (fireDrawAngle `M.lookup` _leadShoulderHipsOffsets cfg)

        legsHipsOffset = fromMaybe zeroPos2 $ do
            offsets <- _status (legsState :: LegsState) `M.lookup` _legsHipsOffsets cfg
            let
                legsSpr = _sprite (legsState :: LegsState)
                idx     = _int (_frameIndex legsSpr) `mod` V.length offsets
            offsets V.!? idx

        angleDelta   = gunFireDrawAngleDelta angle playerDir fireDrawAngle
        headAngle    = angleDelta * _headAngleMultiplier cfg
        torsoAngle   = angleDelta * _torsoAngleMultiplier cfg
        leadArmAngle = angleDelta * _leadArmAngleMultiplier cfg
        rearArmAngle = angleDelta * _rearArmAngleMultiplier cfg

        hipsPos         = playerPos `vecAdd` vecFlip legsHipsOffset playerDir
        neckOffset      = vecFlipRotate torsoNeckOffset playerDir torsoAngle
        neckPos         = hipsPos `vecAdd` neckOffset
        rearShoulderPos = hipsPos `vecAdd` vecFlipRotate rearShoulderHipsOffset playerDir torsoAngle
        leadShoulderPos = hipsPos `vecAdd` vecFlipRotate leadShoulderHipsOffset playerDir torsoAngle

loadLegsSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m LegsSprites
loadLegsSprites =
    LegsSprites <$>
    loadRevolverSprite "revolver-legs.spr" <*>
    loadRevolverSprite "revolver-air-legs.spr" <*>
    loadPackSpr "legs-walk.spr" <*>
    loadPackSpr "legs-back-walk.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/player/player-guns.pack" f

loadMuzzleFlash :: (FileCache m, GraphicsRead m, MonadIO m) => RevolverConfig -> m MuzzleFlash
loadMuzzleFlash cfg = do
    muzzleFlashSprs <- traverse loadRevolverSprite muzzleFlashSpriteFileNames
    return $ mkMuzzleFlash LeadArmMuzzleFlash (_muzzleFlashOffset cfg) muzzleFlashSprs

mkFireDrawData :: forall m. (FileCache m, GraphicsRead m, MonadIO m) => RevolverConfig -> m GunFireDrawData
mkFireDrawData cfg =
    let
        loadFireDrawAngleMap :: (a -> m b) -> [(GunFireDrawAngle, a)] -> m (M.Map GunFireDrawAngle b)
        loadFireDrawAngleMap f angleValues = M.fromList <$> sequenceA
            [(angle,) <$> f value | (angle, value) <- angleValues]
    in do
        armOrders <- loadFireDrawAngleMap pure
            [ (FireDraw90Degrees, DrawRearArmInFront)
            , (FireDraw45Degrees, DrawRearArmInFront)
            , (FireDraw0Degrees, DrawRearArmInFront)
            , (FireDrawNeg45Degrees, DrawRearArmInFront)
            , (FireDrawNeg90Degrees, DrawRearArmInFront)
            , (BackFireDraw90Degrees, DrawLeadArmInFront)
            , (BackFireDraw45Degrees, DrawLeadArmInFront)
            , (BackFireDraw0Degrees, DrawLeadArmInFront)
            , (BackFireDrawNeg45Degrees, DrawLeadArmInFront)
            , (BackFireDrawNeg90Degrees, DrawLeadArmInFront)
            ]

        headSprs <- loadFireDrawAngleMap loadRevolverSprite
            [ (FireDraw90Degrees, "revolver-fire-90-head.spr")
            , (FireDraw45Degrees, "revolver-fire-45-head.spr")
            , (FireDraw0Degrees, "revolver-fire-0-head.spr")
            , (FireDrawNeg45Degrees, "revolver-fire-neg-45-head.spr")
            , (FireDrawNeg90Degrees, "revolver-fire-neg-90-head.spr")
            , (BackFireDraw90Degrees, "revolver-back-fire-90-head.spr")
            , (BackFireDraw45Degrees, "revolver-back-fire-45-head.spr")
            , (BackFireDraw0Degrees, "revolver-back-fire-0-head.spr")
            , (BackFireDrawNeg45Degrees, "revolver-back-fire-neg-45-head.spr")
            , (BackFireDrawNeg90Degrees, "revolver-back-fire-neg-90-head.spr")
            ]

        torsoSprs <- loadFireDrawAngleMap loadRevolverSprite
            [ (FireDraw90Degrees, "revolver-fire-90-torso.spr")
            , (FireDraw45Degrees, "revolver-fire-45-torso.spr")
            , (FireDraw0Degrees, "revolver-fire-0-torso.spr")
            , (FireDrawNeg45Degrees, "revolver-fire-neg-45-torso.spr")
            , (FireDrawNeg90Degrees, "revolver-fire-neg-90-torso.spr")
            , (BackFireDraw90Degrees, "revolver-back-fire-90-torso.spr")
            , (BackFireDraw45Degrees, "revolver-back-fire-45-torso.spr")
            , (BackFireDraw0Degrees, "revolver-back-fire-0-torso.spr")
            , (BackFireDrawNeg45Degrees, "revolver-back-fire-neg-45-torso.spr")
            , (BackFireDrawNeg90Degrees, "revolver-back-fire-neg-90-torso.spr")
            ]

        leadArmSprs <- loadFireDrawAngleMap loadRevolverSprite
            [ (FireDraw90Degrees, "revolver-fire-90-lead-arm.spr")
            , (FireDraw45Degrees, "revolver-fire-45-lead-arm.spr")
            , (FireDraw0Degrees, "revolver-fire-0-lead-arm.spr")
            , (FireDrawNeg45Degrees, "revolver-fire-neg-45-lead-arm.spr")
            , (FireDrawNeg90Degrees, "revolver-fire-neg-90-lead-arm.spr")
            , (BackFireDraw90Degrees, "revolver-back-fire-90-lead-arm.spr")
            , (BackFireDraw45Degrees, "revolver-back-fire-45-lead-arm.spr")
            , (BackFireDraw0Degrees, "revolver-back-fire-0-lead-arm.spr")
            , (BackFireDrawNeg45Degrees, "revolver-back-fire-neg-45-lead-arm.spr")
            , (BackFireDrawNeg90Degrees, "revolver-back-fire-neg-90-lead-arm.spr")
            ]

        rearArmSprs <- loadFireDrawAngleMap loadRevolverSprite
            [ (FireDraw90Degrees, "revolver-fire-90-rear-arm.spr")
            , (FireDraw45Degrees, "revolver-fire-45-rear-arm.spr")
            , (FireDraw0Degrees, "revolver-fire-0-rear-arm.spr")
            , (FireDrawNeg45Degrees, "revolver-fire-neg-45-rear-arm.spr")
            , (FireDrawNeg90Degrees, "revolver-fire-neg-90-rear-arm.spr")
            , (BackFireDraw90Degrees, "revolver-back-fire-90-rear-arm.spr")
            , (BackFireDraw45Degrees, "revolver-back-fire-45-rear-arm.spr")
            , (BackFireDraw0Degrees, "revolver-back-fire-0-rear-arm.spr")
            , (BackFireDrawNeg45Degrees, "revolver-back-fire-neg-45-rear-arm.spr")
            , (BackFireDrawNeg90Degrees, "revolver-back-fire-neg-90-rear-arm.spr")
            ]

        legsSprs    <- loadLegsSprites
        muzzleFlash <- loadMuzzleFlash cfg

        return $ GunFireDrawData
            { _fireDrawAngle          = FireDraw0Degrees
            , _armOrders              = armOrders
            , _headSprites            = headSprs
            , _torsoSprites           = torsoSprs
            , _leadArmSprites         = leadArmSprs
            , _rearArmSprites         = rearArmSprs
            , _legsSprites            = Just legsSprs
            , _muzzleFlash            = Just muzzleFlash
            , _calculatePlayerAimBody = calculatePlayerAimBody cfg
            , _uncancelableSecs       = defaultGunFireDrawStateUncancelableSecs
            }

mkRevolverGun :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some Gun)
mkRevolverGun = do
    revolverData <- mkRevolverData
    let cfg       = _config (revolverData :: RevolverData)
    fireDrawData <- mkFireDrawData cfg

    return . Some $ (mkGun revolverData RevolverGun)
        { _fireDrawData = Just fireDrawData
        , _think        = think
        , _update       = update
        }

update :: ConfigsRead m => GunUpdate RevolverData m
update revolver = do
    cfg             <- readConfig _playerGun _revolver
    let fireDrawData = (\fdd -> fdd {_calculatePlayerAimBody = calculatePlayerAimBody cfg}) <$> _fireDrawData revolver

    return $ revolver
        { _data         = (_data revolver :: RevolverData) {_config = cfg}
        , _fireDrawData = fireDrawData
        }

fireShot :: GunStatus -> InputState -> Player -> Gun RevolverData -> [Msg ThinkPlayerMsgsPhase]
fireShot gunStatus inputState player revolver
    | isShootInput && canShoot && not canSpendMeter && bullets == maxBullets =
        [mkMsg $ UiMsgInsufficientMeter meterCost shootJustPressed]

    | isShootInput && canShoot && canSpendMeter =
        let
            updateShoot = \g -> g
                { _data = (_data g)
                    { _status   = BetweenShotsStatus
                    , _bullets  = bullets - 1
                    , _cooldown = _betweenShotsCd cfg
                    }
                }

            velMsgs
                | inAir && velY > 0.0 = [mkMsgEx (PlayerMsgSetVelocity $ _fireAirVel cfg) MsgAfterNormalOrder]
                | otherwise           = []
            playerMsgs                = velMsgs ++
                [ mkMsg $ PlayerMsgUpdateGun updateShoot
                , mkMsg PlayerMsgFiredGun
                ]
        in playerMsgs ++
            [ mkMsg $ NewThinkProjectileMsgAddM (mkShotProjectile player cfg)
            , mkMsg $ AudioMsgPlaySound revolverSoundPath (_pos player)
            , mkMsg $ PlayerMsgSpendMeter meterCost
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
            , mkMsg $ AudioMsgPlaySound revolverReloadSoundPath (_pos player)
            ]

    | otherwise = decreaseCooldownMsgs

    where
        cfg           = _config (revolverData :: RevolverData)
        meterCost     = _shotMeterCost cfg
        canSpendMeter = canSpendPlayerMeter meterCost player
        canShoot      = gunStatus == ActiveStatus Shootable
        inAir         = not $ _touchingGround (_flags player)
        velY          = vecY $ _vel (player :: Player)
        revolverData  = _data revolver
        bullets       = _bullets revolverData
        cooldown      = _cooldown revolverData
        maxBullets    = _maxBullets cfg

        shootJustPressed = ShootAlias `aliasPressed` inputState
        shootPressed     = shootJustPressed || ShootInput `inPlayerInputBuffer` player
        shootHeld        = ShootAlias `aliasHold` inputState
        isShootInput     = shootPressed || (bullets < _maxBullets cfg && shootHeld)

betweenShots :: Player -> Gun RevolverData -> [Msg ThinkPlayerMsgsPhase]
betweenShots player revolver
    | cooldown <= 0.0 =
        let
            bullets            = _bullets revolverData
            (status', cooldown')
                | bullets <= 0 = (ReloadStatus, _reloadCd cfg)
                | otherwise    = (ReadyStatus, _autoReloadCd cfg)

            soundMsgs
                | status' == ReloadStatus = [mkMsg $ AudioMsgPlaySound revolverReloadSoundPath (_pos player)]
                | otherwise               = []

            updateReload = \g -> g
                { _data = (_data g)
                    { _status   = status'
                    , _cooldown = cooldown'
                    }
                }
            setGunMsg    = mkMsg $ PlayerMsgUpdateGun updateReload
        in setGunMsg:soundMsgs

    | otherwise = decreaseCooldownMsgs

    where
        revolverData = _data revolver
        cooldown     = _cooldown revolverData
        cfg          = _config (revolverData :: RevolverData)

reloadShots :: InputState -> Gun RevolverData -> [Msg ThinkPlayerMsgsPhase]
reloadShots inputState revolver
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
        ReadyStatus        -> fireShot gunStatus inputState player revolver
        BetweenShotsStatus -> betweenShots player revolver
        ReloadStatus       -> reloadShots inputState revolver
