module Player.Gun.All.Shotgun
    ( mkShotgunGun
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))
import Data.Maybe             (fromMaybe)
import qualified Data.Map as M
import qualified Data.Vector as V

import Configs
import Configs.All.PlayerGun
import Configs.All.PlayerGun.Shotgun
import FileCache
import Msg
import Player
import Player.AimBody
import Player.BufferedInputState
import Player.Gun
import Player.Gun.All.Shotgun.BurnShot
import Player.Gun.All.Shotgun.Data
import Player.Gun.All.Shotgun.Shot
import Player.Gun.FireDrawAngle
import Player.Gun.FireDrawData
import Player.Gun.FireDrawState.LegsState
import Util
import Window.Graphics
import Window.InputState

fireSoundPath     = "event:/SFX Events/Player/Guns/shotgun"           :: FilePath
burnFireSoundPath = "event:/SFX Events/Player/Guns/shotgun-burn-shot" :: FilePath
pumpSoundPath     = "event:/SFX Events/Player/Guns/shotgun-pump"      :: FilePath

calculatePlayerAimBody
    :: ShotgunConfig
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
    loadPackSpr "shotgun-legs.spr" <*>
    loadPackSpr "shotgun-air-legs.spr" <*>
    loadPackSpr "legs-walk.spr" <*>
    loadPackSpr "legs-back-walk.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/player/player-guns.pack" f

mkFireDrawData :: forall m. (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => ShotgunData -> m GunFireDrawData
mkFireDrawData shotgunData =
    let
        loadFireDrawAngleMap :: (a -> m b) -> [(GunFireDrawAngle, a)] -> m (M.Map GunFireDrawAngle b)
        loadFireDrawAngleMap f angleValues = M.fromList <$> sequenceA
            [(angle,) <$> f value | (angle, value) <- angleValues]

        loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/player/player-guns.pack" f
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

        headSprs <- loadFireDrawAngleMap loadPackSpr
            [ (FireDraw90Degrees, "shotgun-fire-90-head.spr")
            , (FireDraw45Degrees, "shotgun-fire-45-head.spr")
            , (FireDraw0Degrees, "shotgun-fire-0-head.spr")
            , (FireDrawNeg45Degrees, "shotgun-fire-neg-45-head.spr")
            , (FireDrawNeg90Degrees, "shotgun-fire-neg-90-head.spr")
            , (BackFireDraw90Degrees, "shotgun-back-fire-90-head.spr")
            , (BackFireDraw45Degrees, "shotgun-back-fire-45-head.spr")
            , (BackFireDraw0Degrees, "shotgun-back-fire-0-head.spr")
            , (BackFireDrawNeg45Degrees, "shotgun-back-fire-neg-45-head.spr")
            , (BackFireDrawNeg90Degrees, "shotgun-back-fire-neg-90-head.spr")
            ]

        torsoSprs <- loadFireDrawAngleMap loadPackSpr
            [ (FireDraw90Degrees, "shotgun-fire-90-torso.spr")
            , (FireDraw45Degrees, "shotgun-fire-45-torso.spr")
            , (FireDraw0Degrees, "shotgun-fire-0-torso.spr")
            , (FireDrawNeg45Degrees, "shotgun-fire-neg-45-torso.spr")
            , (FireDrawNeg90Degrees, "shotgun-fire-neg-90-torso.spr")
            , (BackFireDraw90Degrees, "shotgun-back-fire-90-torso.spr")
            , (BackFireDraw45Degrees, "shotgun-back-fire-45-torso.spr")
            , (BackFireDraw0Degrees, "shotgun-back-fire-0-torso.spr")
            , (BackFireDrawNeg45Degrees, "shotgun-back-fire-neg-45-torso.spr")
            , (BackFireDrawNeg90Degrees, "shotgun-back-fire-neg-90-torso.spr")
            ]

        leadArmSprs <- loadFireDrawAngleMap loadPackSpr
            [ (FireDraw90Degrees, "shotgun-fire-90-lead-arm.spr")
            , (FireDraw45Degrees, "shotgun-fire-45-lead-arm.spr")
            , (FireDraw0Degrees, "shotgun-fire-0-lead-arm.spr")
            , (FireDrawNeg45Degrees, "shotgun-fire-neg-45-lead-arm.spr")
            , (FireDrawNeg90Degrees, "shotgun-fire-neg-90-lead-arm.spr")
            , (BackFireDraw90Degrees, "shotgun-back-fire-90-lead-arm.spr")
            , (BackFireDraw45Degrees, "shotgun-back-fire-45-lead-arm.spr")
            , (BackFireDraw0Degrees, "shotgun-back-fire-0-lead-arm.spr")
            , (BackFireDrawNeg45Degrees, "shotgun-back-fire-neg-45-lead-arm.spr")
            , (BackFireDrawNeg90Degrees, "shotgun-back-fire-neg-90-lead-arm.spr")
            ]

        rearArmSprs <- loadFireDrawAngleMap loadPackSpr
            [ (FireDraw90Degrees, "shotgun-fire-90-rear-arm.spr")
            , (FireDraw45Degrees, "shotgun-fire-45-rear-arm.spr")
            , (FireDraw0Degrees, "shotgun-fire-0-rear-arm.spr")
            , (FireDrawNeg45Degrees, "shotgun-fire-neg-45-rear-arm.spr")
            , (FireDrawNeg90Degrees, "shotgun-fire-neg-90-rear-arm.spr")
            , (BackFireDraw90Degrees, "shotgun-back-fire-90-rear-arm.spr")
            , (BackFireDraw45Degrees, "shotgun-back-fire-45-rear-arm.spr")
            , (BackFireDraw0Degrees, "shotgun-back-fire-0-rear-arm.spr")
            , (BackFireDrawNeg45Degrees, "shotgun-back-fire-neg-45-rear-arm.spr")
            , (BackFireDrawNeg90Degrees, "shotgun-back-fire-neg-90-rear-arm.spr")
            ]

        legsSprs <- loadLegsSprites

        return $ GunFireDrawData
            { _fireDrawAngle          = FireDraw0Degrees
            , _armOrders              = armOrders
            , _headSprites            = headSprs
            , _torsoSprites           = torsoSprs
            , _leadArmSprites         = leadArmSprs
            , _rearArmSprites         = rearArmSprs
            , _legsSprites            = Just legsSprs
            , _muzzleFlash            = Just $ _normalMuzzleFlash shotgunData
            , _calculatePlayerAimBody = calculatePlayerAimBody $ _config (shotgunData :: ShotgunData)
            , _uncancelableSecs       = defaultGunFireDrawStateUncancelableSecs
            }

mkShotgunGun :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some Gun)
mkShotgunGun = do
    shotgunData  <- mkShotgunData
    fireDrawData <- mkFireDrawData shotgunData

    return . Some $ (mkGun shotgunData ShotgunGun)
        { _fireDrawData = Just fireDrawData
        , _think        = think
        , _update       = update
        }

thinkReadyStatus :: InputRead m => Gun ShotgunData -> GunStatus -> Player -> m [Msg ThinkPlayerMsgsPhase]
thinkReadyStatus shotgun gunStatus player = thinkRdy <$> readInputState
    where
        thinkRdy :: InputState -> [Msg ThinkPlayerMsgsPhase]
        thinkRdy inputState
            | canShoot && (shootDownPressed || shootPressed) = if
                | not canSpendMeter -> [mkMsg $ UiMsgInsufficientMeter meterCost shootJustPressed]
                | otherwise         ->
                    let
                        updateShoot = \g ->
                            let
                                gData                  = _data g
                                muzzleFlash
                                    | shootDownPressed = _burnMuzzleFlash gData
                                    | otherwise        = _normalMuzzleFlash gData
                                fireDrawData           = _fireDrawData g <&> \fdd ->
                                    fdd {_muzzleFlash = Just muzzleFlash}
                            in g
                                { _data         = gData
                                    { _status   = AfterFireStatus
                                    , _cooldown = _afterFireSecs cfg
                                    }
                                , _fireDrawData = fireDrawData
                                }

                        velMsgs
                            | inAir && velY > 0.0 =
                                [mkMsgEx (PlayerMsgSetVelocity $ _fireAirVel cfg) MsgAfterNormalOrder]
                            | otherwise           = []

                        projs
                            | shootDownPressed = mkBurnShotProjectiles player (_burnShotOwnerId shotgunData) cfg
                            | otherwise        = mkShotProjectiles player cfg

                        soundPath
                            | shootDownPressed = burnFireSoundPath
                            | otherwise        = fireSoundPath
                    in velMsgs ++
                        [ mkMsg $ PlayerMsgUpdateGun updateShoot
                        , mkMsg PlayerMsgFiredGun
                        , mkMsg $ AudioMsgPlaySound soundPath shoulderPos
                        , mkMsg $ NewThinkProjectileMsgAddsM projs
                        , mkMsg $ PlayerMsgSpendMeter meterCost
                        ]

            | otherwise = []

            where
                shootJustPressed = ShootAlias `aliasPressed` inputState
                shootPressed     = shootJustPressed || ShootInput `inPlayerInputBuffer` player
                shootDownPressed =
                    (ShootAlias `aliasPressed` inputState && DownAlias `aliasHold` inputState) ||
                    ShootDownInput `inPlayerInputBuffer` player

                shotgunData   = _data shotgun
                meterCost     = _shotMeterCost $ _config (shotgunData :: ShotgunData)
                canSpendMeter = canSpendPlayerMeter meterCost player
                canShoot      = gunStatus == ActiveStatus Shootable

                shoulderPos = playerShoulderPos player
                inAir       = not $ _touchingGround (_flags player)
                velY        = vecY $ _vel (player :: Player)
                cfg         = _config (_data shotgun :: ShotgunData)

decreaseCooldownMsgs :: [Msg ThinkPlayerMsgsPhase]
decreaseCooldownMsgs = [mkMsg $ PlayerMsgUpdateGun updateCooldown]
    where updateCooldown = \g -> g {_data = shotgunDataDecreaseCd (_data g)}

thinkAfterFireStatus :: Gun ShotgunData -> Player -> [Msg ThinkPlayerMsgsPhase]
thinkAfterFireStatus shotgun player
    | cooldown > 0.0 = decreaseCooldownMsgs
    | otherwise      =
        let
            updateAfterFire = \g -> g
                { _data = (_data g)
                    { _status   = PumpStatus
                    , _cooldown = _pumpSecs cfg
                    }
                }
        in
            [ mkMsg $ PlayerMsgUpdateGun updateAfterFire
            , mkMsg $ AudioMsgPlaySound pumpSoundPath shoulderPos
            ]
    where
        shotgunData = _data shotgun
        cfg         = _config (shotgunData :: ShotgunData)
        cooldown    = _cooldown shotgunData
        shoulderPos = playerShoulderPos player

thinkPumpStatus :: Gun ShotgunData -> [Msg ThinkPlayerMsgsPhase]
thinkPumpStatus shotgun
    | cooldown > 0.0 = decreaseCooldownMsgs
    | otherwise      =
        let
            updatePump = \g -> g
                { _data = (_data g)
                    { _status   = ReadyStatus
                    , _cooldown = 0.0
                    }
                }
        in [mkMsg $ PlayerMsgUpdateGun updatePump]
    where
        shotgunData = _data shotgun
        cooldown    = _cooldown shotgunData

think :: InputRead m => GunThink ShotgunData m
think gunStatus player shotgun = case shotgunStatus of
    ReadyStatus     -> thinkReadyStatus shotgun gunStatus player
    AfterFireStatus -> return $ thinkAfterFireStatus shotgun player
    PumpStatus      -> return $ thinkPumpStatus shotgun
    where shotgunStatus = _status (_data shotgun :: ShotgunData)

update :: ConfigsRead m => GunUpdate ShotgunData m
update shotgun = do
    cfg             <- readConfig _playerGun _shotgun
    let fireDrawData = (\fdd -> fdd {_calculatePlayerAimBody = calculatePlayerAimBody cfg}) <$> _fireDrawData shotgun

    return $ shotgun
        { _data         = (_data shotgun :: ShotgunData) {_config = cfg}
        , _fireDrawData = fireDrawData
        }
