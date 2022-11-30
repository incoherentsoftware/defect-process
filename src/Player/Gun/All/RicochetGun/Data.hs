module Player.Gun.All.RicochetGun.Data
    ( RicochetGunStatus(..)
    , RicochetGunData(..)
    , mkRicochetGunData
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Vector as V

import Configs
import Configs.All.PlayerGun
import Configs.All.PlayerGun.RicochetGun
import FileCache
import Player.AimBody
import Player.Gun.All.RicochetGun.Util
import Player.Gun.FireDrawAngle
import Player.Gun.FireDrawData
import Player.Gun.FireDrawState.LegsState
import Player.Gun.MuzzleFlash
import Util
import Window.Graphics

addBounceUncancelableSecs = 0.2 :: Secs

packPath                   = \f -> PackResourceFilePath "data/player/player-guns.pack" f
uiSegmentBackdropImagePath = packPath "ricochet-gun-ui-segment-backdrop.image" :: PackResourceFilePath
uiSegmentFilledImagePath   = packPath "ricochet-gun-ui-segment-filled.image"   :: PackResourceFilePath

muzzleFlashSpriteFileNames = NE.fromList
    [ "ricochet-gun-muzzle-flash-a.spr"
    , "ricochet-gun-muzzle-flash-b.spr"
    , "ricochet-gun-muzzle-flash-c.spr"
    ] :: NE.NonEmpty FileName

data RicochetGunStatus
    = ReadyStatus
    | BetweenShotsStatus
    deriving (Eq, Show)

data RicochetGunData = RicochetGunData
    { _status                 :: RicochetGunStatus
    , _powerLevel             :: Int
    , _cooldownTtl            :: Secs
    , _showUiOverlayTtl       :: Secs
    , _isPowerUpShootHeld     :: Bool
    , _shootFireDrawData      :: GunFireDrawData
    , _addBounceFireDrawData  :: GunFireDrawData
    , _uiSegmentBackdropImage :: Image
    , _uiSegmentFilledImage   :: Image
    , _config                 :: RicochetGunConfig
    }

loadLegsSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m LegsSprites
loadLegsSprites =
    LegsSprites <$>
    loadRicochetGunSprite "ricochet-gun-legs.spr" <*>
    loadRicochetGunSprite "ricochet-gun-air-legs.spr" <*>
    loadRicochetGunSprite "legs-walk.spr" <*>
    loadRicochetGunSprite "legs-back-walk.spr"

loadMuzzleFlash :: (FileCache m, GraphicsRead m, MonadIO m) => RicochetGunConfig -> m MuzzleFlash
loadMuzzleFlash cfg = do
    muzzleFlashSprs <- traverse loadRicochetGunSprite muzzleFlashSpriteFileNames
    return $ mkMuzzleFlash LeadArmMuzzleFlash (_muzzleFlashOffset cfg) muzzleFlashSprs

calculateShootPlayerAimBody :: RicochetGunConfig -> CalculatePlayerAimBody
calculateShootPlayerAimBody cfg fireDrawAngle playerPos angle playerDir legsState = PlayerAimBody
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
                idx     = _int (_frameIndex legsSpr :: FrameIndex) `mod` V.length offsets
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

mkShootFireDrawData :: forall m. (FileCache m, GraphicsRead m, MonadIO m) => RicochetGunConfig -> m GunFireDrawData
mkShootFireDrawData cfg =
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

        headSprs <- loadFireDrawAngleMap loadRicochetGunSprite
            [ (FireDraw90Degrees, "ricochet-gun-fire-90-head.spr")
            , (FireDraw45Degrees, "ricochet-gun-fire-45-head.spr")
            , (FireDraw0Degrees, "ricochet-gun-fire-0-head.spr")
            , (FireDrawNeg45Degrees, "ricochet-gun-fire-neg-45-head.spr")
            , (FireDrawNeg90Degrees, "ricochet-gun-fire-neg-90-head.spr")
            , (BackFireDraw90Degrees, "ricochet-gun-back-fire-90-head.spr")
            , (BackFireDraw45Degrees, "ricochet-gun-back-fire-45-head.spr")
            , (BackFireDraw0Degrees, "ricochet-gun-back-fire-0-head.spr")
            , (BackFireDrawNeg45Degrees, "ricochet-gun-back-fire-neg-45-head.spr")
            , (BackFireDrawNeg90Degrees, "ricochet-gun-back-fire-neg-90-head.spr")
            ]

        torsoSprs <- loadFireDrawAngleMap loadRicochetGunSprite
            [ (FireDraw90Degrees, "ricochet-gun-fire-90-torso.spr")
            , (FireDraw45Degrees, "ricochet-gun-fire-45-torso.spr")
            , (FireDraw0Degrees, "ricochet-gun-fire-0-torso.spr")
            , (FireDrawNeg45Degrees, "ricochet-gun-fire-neg-45-torso.spr")
            , (FireDrawNeg90Degrees, "ricochet-gun-fire-neg-90-torso.spr")
            , (BackFireDraw90Degrees, "ricochet-gun-back-fire-90-torso.spr")
            , (BackFireDraw45Degrees, "ricochet-gun-back-fire-45-torso.spr")
            , (BackFireDraw0Degrees, "ricochet-gun-back-fire-0-torso.spr")
            , (BackFireDrawNeg45Degrees, "ricochet-gun-back-fire-neg-45-torso.spr")
            , (BackFireDrawNeg90Degrees, "ricochet-gun-back-fire-neg-90-torso.spr")
            ]

        leadArmSprs <- loadFireDrawAngleMap loadRicochetGunSprite
            [ (FireDraw90Degrees, "ricochet-gun-fire-90-lead-arm.spr")
            , (FireDraw45Degrees, "ricochet-gun-fire-45-lead-arm.spr")
            , (FireDraw0Degrees, "ricochet-gun-fire-0-lead-arm.spr")
            , (FireDrawNeg45Degrees, "ricochet-gun-fire-neg-45-lead-arm.spr")
            , (FireDrawNeg90Degrees, "ricochet-gun-fire-neg-90-lead-arm.spr")
            , (BackFireDraw90Degrees, "ricochet-gun-back-fire-90-lead-arm.spr")
            , (BackFireDraw45Degrees, "ricochet-gun-back-fire-45-lead-arm.spr")
            , (BackFireDraw0Degrees, "ricochet-gun-back-fire-0-lead-arm.spr")
            , (BackFireDrawNeg45Degrees, "ricochet-gun-back-fire-neg-45-lead-arm.spr")
            , (BackFireDrawNeg90Degrees, "ricochet-gun-back-fire-neg-90-lead-arm.spr")
            ]

        rearArmSprs <- loadFireDrawAngleMap loadRicochetGunSprite
            [ (FireDraw90Degrees, "ricochet-gun-fire-90-rear-arm.spr")
            , (FireDraw45Degrees, "ricochet-gun-fire-45-rear-arm.spr")
            , (FireDraw0Degrees, "ricochet-gun-fire-0-rear-arm.spr")
            , (FireDrawNeg45Degrees, "ricochet-gun-fire-neg-45-rear-arm.spr")
            , (FireDrawNeg90Degrees, "ricochet-gun-fire-neg-90-rear-arm.spr")
            , (BackFireDraw90Degrees, "ricochet-gun-back-fire-90-rear-arm.spr")
            , (BackFireDraw45Degrees, "ricochet-gun-back-fire-45-rear-arm.spr")
            , (BackFireDraw0Degrees, "ricochet-gun-back-fire-0-rear-arm.spr")
            , (BackFireDrawNeg45Degrees, "ricochet-gun-back-fire-neg-45-rear-arm.spr")
            , (BackFireDrawNeg90Degrees, "ricochet-gun-back-fire-neg-90-rear-arm.spr")
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
            , _calculatePlayerAimBody = calculateShootPlayerAimBody cfg
            , _uncancelableSecs       = defaultGunFireDrawStateUncancelableSecs
            }

calculateAddBouncePlayerAimBody :: RicochetGunConfig -> CalculatePlayerAimBody
calculateAddBouncePlayerAimBody cfg _ playerPos _ playerDir _ = PlayerAimBody
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

mkAddBounceFireDrawData :: forall m. (FileCache m, GraphicsRead m, MonadIO m) => RicochetGunConfig -> m GunFireDrawData
mkAddBounceFireDrawData cfg =
    let
        loadFireDrawAngleMap :: (a -> m b) -> a -> m (M.Map GunFireDrawAngle b)
        loadFireDrawAngleMap f value = M.fromList <$> sequenceA
            [(angle,) <$> f value | angle <- [minBound..maxBound]]
    in do
        armOrders   <- loadFireDrawAngleMap pure DrawLeadArmInFront
        headSprs    <- loadFireDrawAngleMap loadRicochetGunSprite "ricochet-gun-fake-body-part.spr"
        leadArmSprs <- loadFireDrawAngleMap loadRicochetGunSprite "ricochet-gun-fake-body-part.spr"
        rearArmSprs <- loadFireDrawAngleMap loadRicochetGunSprite "ricochet-gun-fake-body-part.spr"
        torsoSprs   <- loadFireDrawAngleMap loadRicochetGunSprite "ricochet-gun-add-bounce.spr"

        legsSprs <- loadLegsSprites >>= \ls -> do
            standSpr <- loadRicochetGunSprite "ricochet-gun-legs-add-bounce.spr"
            airSpr   <- loadRicochetGunSprite "ricochet-gun-air-legs-add-bounce.spr"
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
            , _calculatePlayerAimBody = calculateAddBouncePlayerAimBody cfg
            , _uncancelableSecs       = addBounceUncancelableSecs
            }

mkRicochetGunData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m RicochetGunData
mkRicochetGunData = do
    cfg                   <- readConfig _playerGun _ricochetGun
    shootFireDrawData     <- mkShootFireDrawData cfg
    addBounceFireDrawData <- mkAddBounceFireDrawData cfg
    uiSegmentBackdropImg  <- loadPackImage uiSegmentBackdropImagePath
    uiSegmentFilledImg    <- loadPackImage uiSegmentFilledImagePath

    return $ RicochetGunData
        { _status                    = ReadyStatus
        , _powerLevel                = 0
        , _cooldownTtl               = 0.0
        , _showUiOverlayTtl          = 0.0
        , _isPowerUpShootHeld        = False
        , _shootFireDrawData         = shootFireDrawData
        , _addBounceFireDrawData     = addBounceFireDrawData
        , _uiSegmentBackdropImage    = uiSegmentBackdropImg
        , _uiSegmentFilledImage      = uiSegmentFilledImg
        , _config                    = cfg
        }
