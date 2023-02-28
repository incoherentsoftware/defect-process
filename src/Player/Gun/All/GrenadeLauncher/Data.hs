module Player.Gun.All.GrenadeLauncher.Data
    ( GrenadeLauncherData(..)
    , mkGrenadeLauncherData
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Vector as V

import Configs
import Configs.All.PlayerGun
import Configs.All.PlayerGun.GrenadeLauncher
import FileCache
import Player.AimBody
import Player.Gun.FireDrawAngle
import Player.Gun.FireDrawData
import Player.Gun.FireDrawState.LegsState
import Player.Gun.MuzzleFlash
import Util
import Window.Graphics

smokeSpriteFileNames = NE.fromList
    [ "grenade-launcher-smoke-a.spr"
    , "grenade-launcher-smoke-b.spr"
    , "grenade-launcher-smoke-c.spr"
    ] :: NE.NonEmpty FileName

throwMineUncancelableSecs = 0.2 :: Secs

data GrenadeLauncherData = GrenadeLauncherData
    { _fireDelayTtl          :: Secs
    , _shootFireDrawData     :: GunFireDrawData
    , _throwMineFireDrawData :: GunFireDrawData
    , _config                :: GrenadeLauncherConfig
    }

mkGrenadeLauncherData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m GrenadeLauncherData
mkGrenadeLauncherData = do
    cfg                   <- readConfig _playerGun _grenadeLauncher
    shootFireDrawData     <- mkShootFireDrawData cfg
    throwMineFireDrawData <- mkThrowMineFireDrawData cfg

    return $ GrenadeLauncherData
        { _fireDelayTtl          = 0.0
        , _shootFireDrawData     = shootFireDrawData
        , _throwMineFireDrawData = throwMineFireDrawData
        , _config                = cfg
        }

loadGrenadeLauncherSprite :: (FileCache m, GraphicsRead m, MonadIO m) => FilePath -> m Sprite
loadGrenadeLauncherSprite f = loadPackSprite $ PackResourceFilePath "data/player/player-guns.pack" f

calculatePlayerAimBody
    :: GrenadeLauncherConfig
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

mkShootFireDrawData :: forall m. (FileCache m, GraphicsRead m, MonadIO m) => GrenadeLauncherConfig -> m GunFireDrawData
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

        headSprs <- loadFireDrawAngleMap loadGrenadeLauncherSprite
            [ (FireDraw90Degrees, "grenade-launcher-fire-90-head.spr")
            , (FireDraw45Degrees, "grenade-launcher-fire-45-head.spr")
            , (FireDraw0Degrees, "grenade-launcher-fire-0-head.spr")
            , (FireDrawNeg45Degrees, "grenade-launcher-fire-neg-45-head.spr")
            , (FireDrawNeg90Degrees, "grenade-launcher-fire-neg-90-head.spr")
            , (BackFireDraw90Degrees, "grenade-launcher-back-fire-90-head.spr")
            , (BackFireDraw45Degrees, "grenade-launcher-back-fire-45-head.spr")
            , (BackFireDraw0Degrees, "grenade-launcher-back-fire-0-head.spr")
            , (BackFireDrawNeg45Degrees, "grenade-launcher-back-fire-neg-45-head.spr")
            , (BackFireDrawNeg90Degrees, "grenade-launcher-back-fire-neg-90-head.spr")
            ]

        torsoSprs <- loadFireDrawAngleMap loadGrenadeLauncherSprite
            [ (FireDraw90Degrees, "grenade-launcher-fire-90-torso.spr")
            , (FireDraw45Degrees, "grenade-launcher-fire-45-torso.spr")
            , (FireDraw0Degrees, "grenade-launcher-fire-0-torso.spr")
            , (FireDrawNeg45Degrees, "grenade-launcher-fire-neg-45-torso.spr")
            , (FireDrawNeg90Degrees, "grenade-launcher-fire-neg-90-torso.spr")
            , (BackFireDraw90Degrees, "grenade-launcher-back-fire-90-torso.spr")
            , (BackFireDraw45Degrees, "grenade-launcher-back-fire-45-torso.spr")
            , (BackFireDraw0Degrees, "grenade-launcher-back-fire-0-torso.spr")
            , (BackFireDrawNeg45Degrees, "grenade-launcher-back-fire-neg-45-torso.spr")
            , (BackFireDrawNeg90Degrees, "grenade-launcher-back-fire-neg-90-torso.spr")
            ]

        leadArmSprs <- loadFireDrawAngleMap loadGrenadeLauncherSprite
            [ (FireDraw90Degrees, "grenade-launcher-fire-90-lead-arm.spr")
            , (FireDraw45Degrees, "grenade-launcher-fire-45-lead-arm.spr")
            , (FireDraw0Degrees, "grenade-launcher-fire-0-lead-arm.spr")
            , (FireDrawNeg45Degrees, "grenade-launcher-fire-neg-45-lead-arm.spr")
            , (FireDrawNeg90Degrees, "grenade-launcher-fire-neg-90-lead-arm.spr")
            , (BackFireDraw90Degrees, "grenade-launcher-back-fire-90-lead-arm.spr")
            , (BackFireDraw45Degrees, "grenade-launcher-back-fire-45-lead-arm.spr")
            , (BackFireDraw0Degrees, "grenade-launcher-back-fire-0-lead-arm.spr")
            , (BackFireDrawNeg45Degrees, "grenade-launcher-back-fire-neg-45-lead-arm.spr")
            , (BackFireDrawNeg90Degrees, "grenade-launcher-back-fire-neg-90-lead-arm.spr")
            ]

        rearArmSprs <- loadFireDrawAngleMap loadGrenadeLauncherSprite
            [ (FireDraw90Degrees, "grenade-launcher-fire-90-rear-arm.spr")
            , (FireDraw45Degrees, "grenade-launcher-fire-45-rear-arm.spr")
            , (FireDraw0Degrees, "grenade-launcher-fire-0-rear-arm.spr")
            , (FireDrawNeg45Degrees, "grenade-launcher-fire-neg-45-rear-arm.spr")
            , (FireDrawNeg90Degrees, "grenade-launcher-fire-neg-90-rear-arm.spr")
            , (BackFireDraw90Degrees, "grenade-launcher-back-fire-90-rear-arm.spr")
            , (BackFireDraw45Degrees, "grenade-launcher-back-fire-45-rear-arm.spr")
            , (BackFireDraw0Degrees, "grenade-launcher-back-fire-0-rear-arm.spr")
            , (BackFireDrawNeg45Degrees, "grenade-launcher-back-fire-neg-45-rear-arm.spr")
            , (BackFireDrawNeg90Degrees, "grenade-launcher-back-fire-neg-90-rear-arm.spr")
            ]

        legsSprs  <- loadLegsSprites
        smokeSprs <- traverse loadGrenadeLauncherSprite smokeSpriteFileNames
        let
            muzzleFlashOffset = _muzzleFlashOffset cfg
            muzzleFlash       = mkMuzzleFlash RearArmMuzzleFlash muzzleFlashOffset smokeSprs

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

calculateThrowMinePlayerAimBody :: GrenadeLauncherConfig -> CalculatePlayerAimBody
calculateThrowMinePlayerAimBody cfg _ playerPos _ playerDir _ = PlayerAimBody
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

mkThrowMineFireDrawData
    :: forall m. (FileCache m, GraphicsRead m, MonadIO m)
    => GrenadeLauncherConfig
    -> m GunFireDrawData
mkThrowMineFireDrawData cfg =
    let
        loadFireDrawAngleMap :: (a -> m b) -> a -> m (M.Map GunFireDrawAngle b)
        loadFireDrawAngleMap f value = M.fromList <$> sequenceA
            [(angle,) <$> f value | angle <- [minBound..maxBound]]
    in do
        armOrders   <- loadFireDrawAngleMap pure DrawLeadArmInFront
        headSprs    <- loadFireDrawAngleMap loadGrenadeLauncherSprite "grenade-launcher-fake-body-part.spr"
        leadArmSprs <- loadFireDrawAngleMap loadGrenadeLauncherSprite "grenade-launcher-fake-body-part.spr"
        rearArmSprs <- loadFireDrawAngleMap loadGrenadeLauncherSprite "grenade-launcher-fake-body-part.spr"
        torsoSprs   <- loadFireDrawAngleMap loadGrenadeLauncherSprite "grenade-launcher-throw-mine.spr"

        legsSprs <- loadLegsSprites >>= \ls -> do
            standSpr <- loadGrenadeLauncherSprite "grenade-launcher-legs-throw-mine.spr"
            airSpr   <- loadGrenadeLauncherSprite "grenade-launcher-air-legs-throw-mine.spr"
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
            , _calculatePlayerAimBody = calculateThrowMinePlayerAimBody cfg
            , _uncancelableSecs       = throwMineUncancelableSecs
            }
