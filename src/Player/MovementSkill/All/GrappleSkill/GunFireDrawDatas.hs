module Player.MovementSkill.All.GrappleSkill.GunFireDrawDatas
    ( GrappleGunFireDrawDatas(..)
    , mkGrappleGunFireDrawDatas
    , updateGrappleGunFireDrawDatas
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.Map as M
import qualified Data.Vector as V

import Configs
import Configs.All.PlayerGun
import Configs.All.PlayerSkill
import Configs.All.PlayerSkill.Grapple
import FileCache
import Player.AimBody
import Player.Gun.FireDrawAngle
import Player.Gun.FireDrawData
import Player.Gun.FireDrawState.LegsState
import Util
import Window.Graphics
import qualified Player.Gun.All.Revolver.Util as Revolver

loadGrappleSprite :: (FileCache m, GraphicsRead m, MonadIO m) => FilePath -> m Sprite
loadGrappleSprite fileName = loadPackSprite $ PackResourceFilePath "data/player/player-skills.pack" fileName

data GrappleGunFireDrawDatas = GrappleGunFireDrawDatas
    { _throw :: GunFireDrawData
    , _pull  :: GunFireDrawData
    }

loadFireDrawAngleMap :: (ConfigsRead m) => (a -> m b) -> [(GunFireDrawAngle, a)] -> m (M.Map GunFireDrawAngle b)
loadFireDrawAngleMap f angleValues = M.fromList <$> sequenceA
    [(angle,) <$> f value | (angle, value) <- angleValues]

loadThrowLegsSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m LegsSprites
loadThrowLegsSprites =
    LegsSprites <$>
    loadGrappleSprite "grapple-throw-legs.spr" <*>
    loadGrappleSprite "grapple-throw-air-legs.spr" <*>
    loadPackSpr "legs-walk.spr" <*>
    loadPackSpr "legs-back-walk.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/player/player-guns.pack" f

mkGrappleThrowGunFireDrawData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m GunFireDrawData
mkGrappleThrowGunFireDrawData = do
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

    headSprs <- loadFireDrawAngleMap loadGrappleSprite
        [ (FireDraw90Degrees, "grapple-throw-fire-90-head.spr")
        , (FireDraw45Degrees, "grapple-throw-fire-45-head.spr")
        , (FireDraw0Degrees, "grapple-throw-fire-0-head.spr")
        , (FireDrawNeg45Degrees, "grapple-throw-fire-neg-45-head.spr")
        , (FireDrawNeg90Degrees, "grapple-throw-fire-neg-90-head.spr")
        , (BackFireDraw90Degrees, "grapple-throw-back-fire-90-head.spr")
        , (BackFireDraw45Degrees, "grapple-throw-back-fire-45-head.spr")
        , (BackFireDraw0Degrees, "grapple-throw-back-fire-0-head.spr")
        , (BackFireDrawNeg45Degrees, "grapple-throw-back-fire-neg-45-head.spr")
        , (BackFireDrawNeg90Degrees, "grapple-throw-back-fire-neg-90-head.spr")
        ]

    torsoSprs <- loadFireDrawAngleMap loadGrappleSprite
        [ (FireDraw90Degrees, "grapple-throw-fire-90-torso.spr")
        , (FireDraw45Degrees, "grapple-throw-fire-45-torso.spr")
        , (FireDraw0Degrees, "grapple-throw-fire-0-torso.spr")
        , (FireDrawNeg45Degrees, "grapple-throw-fire-neg-45-torso.spr")
        , (FireDrawNeg90Degrees, "grapple-throw-fire-neg-90-torso.spr")
        , (BackFireDraw90Degrees, "grapple-throw-back-fire-90-torso.spr")
        , (BackFireDraw45Degrees, "grapple-throw-back-fire-45-torso.spr")
        , (BackFireDraw0Degrees, "grapple-throw-back-fire-0-torso.spr")
        , (BackFireDrawNeg45Degrees, "grapple-throw-back-fire-neg-45-torso.spr")
        , (BackFireDrawNeg90Degrees, "grapple-throw-back-fire-neg-90-torso.spr")
        ]

    leadArmSprs <- loadFireDrawAngleMap loadGrappleSprite
        [ (FireDraw90Degrees, "grapple-throw-fire-90-lead-arm.spr")
        , (FireDraw45Degrees, "grapple-throw-fire-45-lead-arm.spr")
        , (FireDraw0Degrees, "grapple-throw-fire-0-lead-arm.spr")
        , (FireDrawNeg45Degrees, "grapple-throw-fire-neg-45-lead-arm.spr")
        , (FireDrawNeg90Degrees, "grapple-throw-fire-neg-90-lead-arm.spr")
        , (BackFireDraw90Degrees, "grapple-throw-back-fire-90-lead-arm.spr")
        , (BackFireDraw45Degrees, "grapple-throw-back-fire-45-lead-arm.spr")
        , (BackFireDraw0Degrees, "grapple-throw-back-fire-0-lead-arm.spr")
        , (BackFireDrawNeg45Degrees, "grapple-throw-back-fire-neg-45-lead-arm.spr")
        , (BackFireDrawNeg90Degrees, "grapple-throw-back-fire-neg-90-lead-arm.spr")
        ]

    rearArmSprs <- loadFireDrawAngleMap loadGrappleSprite
        [ (FireDraw90Degrees, "grapple-throw-fire-90-rear-arm.spr")
        , (FireDraw45Degrees, "grapple-throw-fire-45-rear-arm.spr")
        , (FireDraw0Degrees, "grapple-throw-fire-0-rear-arm.spr")
        , (FireDrawNeg45Degrees, "grapple-throw-fire-neg-45-rear-arm.spr")
        , (FireDrawNeg90Degrees, "grapple-throw-fire-neg-90-rear-arm.spr")
        , (BackFireDraw90Degrees, "grapple-throw-back-fire-90-rear-arm.spr")
        , (BackFireDraw45Degrees, "grapple-throw-back-fire-45-rear-arm.spr")
        , (BackFireDraw0Degrees, "grapple-throw-back-fire-0-rear-arm.spr")
        , (BackFireDrawNeg45Degrees, "grapple-throw-back-fire-neg-45-rear-arm.spr")
        , (BackFireDrawNeg90Degrees, "grapple-throw-back-fire-neg-90-rear-arm.spr")
        ]

    legsSprs     <- loadThrowLegsSprites
    playerGunCfg <- _playerGun <$> readConfigs

    return $ GunFireDrawData
        { _fireDrawAngle          = FireDraw0Degrees
        , _armOrders              = armOrders
        , _headSprites            = headSprs
        , _torsoSprites           = torsoSprs
        , _leadArmSprites         = leadArmSprs
        , _rearArmSprites         = rearArmSprs
        , _legsSprites            = Just legsSprs
        , _muzzleFlash            = Nothing
        , _calculatePlayerAimBody = Revolver.calculatePlayerAimBody $ _revolver playerGunCfg
        , _uncancelableSecs       = defaultGunFireDrawStateUncancelableSecs
        }

calculatePullPlayerAimBody :: GrappleConfig -> CalculatePlayerAimBody
calculatePullPlayerAimBody cfg fireDrawAngle playerPos angle playerDir legsState = PlayerAimBody
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
        torsoNeckOffset        = fromMaybe zeroPos2 (fireDrawAngle `M.lookup` _pullTorsoNeckOffsets cfg)
        rearShoulderHipsOffset = fromMaybe zeroPos2 (fireDrawAngle `M.lookup` _pullRearShoulderHipsOffsets cfg)
        leadShoulderHipsOffset = fromMaybe zeroPos2 (fireDrawAngle `M.lookup` _pullLeadShoulderHipsOffsets cfg)

        legsHipsOffset = fromMaybe zeroPos2 $ do
            offsets <- _status (legsState :: LegsState) `M.lookup` _pullLegsHipsOffsets cfg
            let
                legsSpr = _sprite (legsState :: LegsState)
                idx     = _int (_frameIndex legsSpr) `mod` V.length offsets
            offsets V.!? idx

        angleDelta   = gunFireDrawAngleDelta angle playerDir fireDrawAngle
        headAngle    = angleDelta * _pullHeadAngleMultiplier cfg
        torsoAngle   = angleDelta * _pullTorsoAngleMultiplier cfg
        leadArmAngle = angleDelta * _pullLeadArmAngleMultiplier cfg
        rearArmAngle = angleDelta * _pullRearArmAngleMultiplier cfg

        hipsPos         = playerPos `vecAdd` vecFlip legsHipsOffset playerDir
        neckOffset      = vecFlipRotate torsoNeckOffset playerDir torsoAngle
        neckPos         = hipsPos `vecAdd` neckOffset
        rearShoulderPos = hipsPos `vecAdd` vecFlipRotate rearShoulderHipsOffset playerDir torsoAngle
        leadShoulderPos = hipsPos `vecAdd` vecFlipRotate leadShoulderHipsOffset playerDir torsoAngle

loadPullLegsSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m LegsSprites
loadPullLegsSprites =
    LegsSprites <$>
    loadGrappleSprite "grapple-pull-legs.spr" <*>
    loadGrappleSprite "grapple-pull-air-legs.spr" <*>
    loadPackSpr "legs-walk.spr" <*>
    loadPackSpr "legs-back-walk.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/player/player-guns.pack" f

mkGrapplePullGunFireDrawData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m GunFireDrawData
mkGrapplePullGunFireDrawData = do
    armOrders <- loadFireDrawAngleMap pure
        [ (FireDraw90Degrees, DrawRearArmInFront)
        , (FireDraw45Degrees, DrawRearArmInFront)
        , (FireDraw0Degrees, DrawRearArmInFront)
        , (FireDrawNeg45Degrees, DrawRearArmInFront)
        , (FireDrawNeg90Degrees, DrawRearArmInFront)
        ]

    headSprs <- loadFireDrawAngleMap loadGrappleSprite
        [ (FireDraw90Degrees, "grapple-pull-fire-90-head.spr")
        , (FireDraw45Degrees, "grapple-pull-fire-45-head.spr")
        , (FireDraw0Degrees, "grapple-pull-fire-0-head.spr")
        , (FireDrawNeg45Degrees, "grapple-pull-fire-neg-45-head.spr")
        , (FireDrawNeg90Degrees, "grapple-pull-fire-neg-90-head.spr")
        ]

    torsoSprs <- loadFireDrawAngleMap loadGrappleSprite
        [ (FireDraw90Degrees, "grapple-pull-fire-90-torso.spr")
        , (FireDraw45Degrees, "grapple-pull-fire-45-torso.spr")
        , (FireDraw0Degrees, "grapple-pull-fire-0-torso.spr")
        , (FireDrawNeg45Degrees, "grapple-pull-fire-neg-45-torso.spr")
        , (FireDrawNeg90Degrees, "grapple-pull-fire-neg-90-torso.spr")
        ]

    leadArmSprs <- loadFireDrawAngleMap loadGrappleSprite
        [ (FireDraw90Degrees, "grapple-pull-fire-90-lead-arm.spr")
        , (FireDraw45Degrees, "grapple-pull-fire-45-lead-arm.spr")
        , (FireDraw0Degrees, "grapple-pull-fire-0-lead-arm.spr")
        , (FireDrawNeg45Degrees, "grapple-pull-fire-neg-45-lead-arm.spr")
        , (FireDrawNeg90Degrees, "grapple-pull-fire-neg-90-lead-arm.spr")
        ]

    rearArmSprs <- loadFireDrawAngleMap loadGrappleSprite
        [ (FireDraw90Degrees, "grapple-pull-fire-90-rear-arm.spr")
        , (FireDraw45Degrees, "grapple-pull-fire-45-rear-arm.spr")
        , (FireDraw0Degrees, "grapple-pull-fire-0-rear-arm.spr")
        , (FireDrawNeg45Degrees, "grapple-pull-fire-neg-45-rear-arm.spr")
        , (FireDrawNeg90Degrees, "grapple-pull-fire-neg-90-rear-arm.spr")
        ]

    legsSprs <- loadPullLegsSprites
    cfg      <- readConfig _playerSkill _grapple

    return $ GunFireDrawData
        { _fireDrawAngle          = FireDraw0Degrees
        , _armOrders              = armOrders
        , _headSprites            = headSprs
        , _torsoSprites           = torsoSprs
        , _leadArmSprites         = leadArmSprs
        , _rearArmSprites         = rearArmSprs
        , _legsSprites            = Just legsSprs
        , _muzzleFlash            = Nothing
        , _calculatePlayerAimBody = calculatePullPlayerAimBody cfg
        , _uncancelableSecs       = defaultGunFireDrawStateUncancelableSecs
        }

mkGrappleGunFireDrawDatas :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m GrappleGunFireDrawDatas
mkGrappleGunFireDrawDatas =
    GrappleGunFireDrawDatas <$>
    mkGrappleThrowGunFireDrawData <*>
    mkGrapplePullGunFireDrawData

updateGrappleGunFireDrawDatas :: ConfigsRead m => GrappleGunFireDrawDatas -> m GrappleGunFireDrawDatas
updateGrappleGunFireDrawDatas fireDrawDatas = do
    cfg <- readConfig _playerSkill _grapple
    return $ fireDrawDatas
        { _pull = (_pull fireDrawDatas) {_calculatePlayerAimBody = calculatePullPlayerAimBody cfg}
        }
