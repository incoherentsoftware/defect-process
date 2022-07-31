module Player.Gun.All.Revolver.Data
    ( RevolverStatus(..)
    , RevolverData(..)
    , mkRevolverData
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

import Configs
import Configs.All.PlayerGun
import Configs.All.PlayerGun.Revolver
import FileCache
import Id
import Player.AimBody
import Player.Gun.All.Revolver.Util
import Player.Gun.FireDrawAngle
import Player.Gun.FireDrawData
import Player.Gun.FireDrawState.LegsState
import Player.Gun.MuzzleFlash
import Util
import Window.Graphics

continuousChargeUncancelableSecs = 0.715 :: Secs
continuousCancelUncancelableSecs = 0.0   :: Secs

normalMuzzleFlashSpriteFileNames = NE.fromList
    [ "revolver-muzzle-flash-a.spr"
    , "revolver-muzzle-flash-b.spr"
    , "revolver-muzzle-flash-c.spr"
    , "revolver-muzzle-flash-d.spr"
    ] :: NE.NonEmpty FileName

continuousMuzzleFlashSpriteFileNames = NE.fromList
    [ "revolver-continuous-muzzle-flash-a.spr"
    , "revolver-continuous-muzzle-flash-b.spr"
    , "revolver-continuous-muzzle-flash-c.spr"
    ] :: NE.NonEmpty FileName

data RevolverStatus
    = ReadyStatus
    | NormalShotsStatus
    | ContinuousShotsStatus
    | ReloadStatus
    deriving (Eq, Show)

data RevolverData = RevolverData
    { _status                       :: RevolverStatus
    , _bullets                      :: Int
    , _cooldown                     :: Float
    , _shootFireDrawData            :: GunFireDrawData
    , _continuousChargeFireDrawData :: GunFireDrawData
    , _continuousCancelFireDrawData :: GunFireDrawData
    , _normalMuzzleFlash            :: MuzzleFlash
    , _continuousMuzzleFlash        :: MuzzleFlash
    , _chargeSoundHashedId          :: HashedId
    , _config                       :: RevolverConfig
    }

loadLegsSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m LegsSprites
loadLegsSprites =
    LegsSprites <$>
    loadRevolverSprite "revolver-legs.spr" <*>
    loadRevolverSprite "revolver-air-legs.spr" <*>
    loadPackSpr "legs-walk.spr" <*>
    loadPackSpr "legs-back-walk.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/player/player-guns.pack" f

mkShootFireDrawData
    :: forall m. (FileCache m, GraphicsRead m, MonadIO m)
    => MuzzleFlash
    -> RevolverConfig
    -> m GunFireDrawData
mkShootFireDrawData normalMuzzleFlash cfg =
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

        legsSprs <- loadLegsSprites

        return $ GunFireDrawData
            { _fireDrawAngle          = FireDraw0Degrees
            , _armOrders              = armOrders
            , _headSprites            = headSprs
            , _torsoSprites           = torsoSprs
            , _leadArmSprites         = leadArmSprs
            , _rearArmSprites         = rearArmSprs
            , _legsSprites            = Just legsSprs
            , _muzzleFlash            = Just normalMuzzleFlash
            , _calculatePlayerAimBody = calculatePlayerAimBody cfg
            , _uncancelableSecs       = defaultGunFireDrawStateUncancelableSecs
            }

calculateContinuousChargePlayerAimBody :: RevolverConfig -> CalculatePlayerAimBody
calculateContinuousChargePlayerAimBody cfg _ playerPos _ playerDir _ = PlayerAimBody
    { _headAngle       = 0.0
    , _neckPos         = hipsPos
    , _torsoAngle      = 0.0
    , _leadShoulderPos = hipsPos
    , _rearShoulderPos = hipsPos
    , _hipsPos         = hipsPos
    , _leadArmAngle    = 0.0
    , _rearArmAngle    = 0.0
    , _aimDir          = playerDir
    }
    where hipsPos = playerPos `vecAdd` vecFlip (_fakeLegsHipsOffset cfg) playerDir

mkContinuousChargeFireDrawData
    :: forall m. (FileCache m, GraphicsRead m, MonadIO m)
    => RevolverConfig
    -> m GunFireDrawData
mkContinuousChargeFireDrawData cfg =
    let
        loadFireDrawAngleMap :: (a -> m b) -> a -> m (M.Map GunFireDrawAngle b)
        loadFireDrawAngleMap f value = M.fromList <$> sequenceA
            [(angle,) <$> f value | angle <- [minBound..maxBound]]
    in do
        armOrders   <- loadFireDrawAngleMap pure DrawLeadArmInFront
        headSprs    <- loadFireDrawAngleMap loadRevolverSprite "revolver-fake-body-part.spr"
        leadArmSprs <- loadFireDrawAngleMap loadRevolverSprite "revolver-fake-body-part.spr"
        rearArmSprs <- loadFireDrawAngleMap loadRevolverSprite "revolver-fake-body-part.spr"
        torsoSprs   <- loadFireDrawAngleMap loadRevolverSprite "revolver-continuous-charge.spr"

        legsSprs <- loadLegsSprites >>= \ls -> do
            standSpr <- loadRevolverSprite "revolver-legs-continuous.spr"
            airSpr   <- loadRevolverSprite "revolver-air-legs-continuous.spr"
            return $ ls
                { _stand = standSpr
                , _air   = airSpr
                }

        return $ GunFireDrawData
            { _fireDrawAngle          = FireDraw0Degrees
            , _armOrders              = armOrders
            , _headSprites            = headSprs
            , _torsoSprites           = torsoSprs
            , _leadArmSprites         = leadArmSprs
            , _rearArmSprites         = rearArmSprs
            , _legsSprites            = Just legsSprs
            , _muzzleFlash            = Nothing
            , _calculatePlayerAimBody = calculateContinuousChargePlayerAimBody cfg
            , _uncancelableSecs       = continuousChargeUncancelableSecs
            }

mkContinuousCancelFireDrawData
    :: forall m. (FileCache m, GraphicsRead m, MonadIO m)
    => RevolverConfig
    -> m GunFireDrawData
mkContinuousCancelFireDrawData cfg =
    let
        loadFireDrawAngleMap :: (a -> m b) -> a -> m (M.Map GunFireDrawAngle b)
        loadFireDrawAngleMap f value = M.fromList <$> sequenceA
            [(angle,) <$> f value | angle <- [minBound..maxBound]]
    in do
        fireDrawData <- mkContinuousChargeFireDrawData cfg
        torsoSprs    <- loadFireDrawAngleMap loadRevolverSprite "revolver-continuous-cancel.spr"

        return $ fireDrawData
            { _torsoSprites     = torsoSprs
            , _uncancelableSecs = continuousCancelUncancelableSecs
            }

loadNormalMuzzleFlash :: (FileCache m, GraphicsRead m, MonadIO m) => RevolverConfig -> m MuzzleFlash
loadNormalMuzzleFlash cfg = do
    muzzleFlashSprs <- traverse loadRevolverSprite normalMuzzleFlashSpriteFileNames
    return $ mkMuzzleFlash LeadArmMuzzleFlash (_muzzleFlashOffset cfg) muzzleFlashSprs

loadContinuousMuzzleFlash :: (FileCache m, GraphicsRead m, MonadIO m) => RevolverConfig -> m MuzzleFlash
loadContinuousMuzzleFlash cfg = do
    muzzleFlashSprs <- traverse loadRevolverSprite continuousMuzzleFlashSpriteFileNames
    return $ mkMuzzleFlash LeadArmMuzzleFlash (_muzzleFlashOffset cfg) muzzleFlashSprs

mkRevolverData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m RevolverData
mkRevolverData = do
    cfg                          <- readConfig _playerGun _revolver
    normalMuzzleFlash            <- loadNormalMuzzleFlash cfg
    continuousMuzzleFlash        <- loadContinuousMuzzleFlash cfg
    shootFireDrawData            <- mkShootFireDrawData normalMuzzleFlash cfg
    continuousChargeFireDrawData <- mkContinuousChargeFireDrawData cfg
    continuousCancelFireDrawData <- mkContinuousCancelFireDrawData cfg
    chargeSoundHashedId          <- hashId <$> newId

    return $ RevolverData
        { _status                       = ReadyStatus
        , _bullets                      = _maxBullets cfg
        , _cooldown                     = 0.0
        , _shootFireDrawData            = shootFireDrawData
        , _continuousChargeFireDrawData = continuousChargeFireDrawData
        , _continuousCancelFireDrawData = continuousCancelFireDrawData
        , _normalMuzzleFlash            = normalMuzzleFlash
        , _continuousMuzzleFlash        = continuousMuzzleFlash
        , _chargeSoundHashedId          = chargeSoundHashedId
        , _config                       = cfg
        }
