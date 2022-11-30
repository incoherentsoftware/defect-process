module Player.Gun.All.RicochetGun.Util
    ( loadRicochetGunSprite
    , readShotBounceEnemyPositions
    , shotBounceReflectOffSurfaceTargetPos
    , shotBounceToSurfaceTargetPos
    , calculatePlayerAimBody
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe             (fromMaybe)
import System.Random.Shuffle  (shuffleM)
import qualified Data.Map as M
import qualified Data.Vector as V

import Collision.Hitbox
import Configs.All.PlayerGun.RicochetGun
import FileCache
import Id
import Msg
import Player.AimBody
import Player.Gun.All.RicochetGun.Shot.Data
import Player.Gun.FireDrawAngle
import Player.Gun.FireDrawState.LegsState
import Util
import Window.Graphics

loadRicochetGunSprite :: (FileCache m, GraphicsRead m, MonadIO m) => FileName -> m Sprite
loadRicochetGunSprite fileName = loadPackSprite $ PackResourceFilePath "data/player/player-guns.pack" fileName

readShotBounceEnemyPositions
    :: (MonadIO m, MsgsRead ThinkProjectileMsgsPhase m)
    => Pos2
    -> ShotPrevBounceData
    -> RicochetGunConfig
    -> m [Pos2]
readShotBounceEnemyPositions shotEndPos prevBounceData cfg =
    let
        prevBounceEnemyId = case prevBounceData of
            PrevBounceEnemyId enemyId -> enemyId
            _                         -> NullId

        processMsg :: InfoMsgPayload -> [Pos2] -> [Pos2]
        processMsg p enemyPositions = case p of
            InfoMsgEnemyPos enemyHbx enemyId
                | enemyId /= prevBounceEnemyId -> hitboxCenter enemyHbx:enemyPositions
            _                                  -> enemyPositions

        inBounceRange = \pos -> vecDistSq shotEndPos pos <= (_maxBounceRange cfg)^2
    in do
        enemyPositions <- foldr processMsg [] <$> readMsgs
        liftIO $ shuffleM (filter inBounceRange enemyPositions)

shotBounceReflectOffSurfaceTargetPos :: Hitbox -> ShotPrevBounceData -> RicochetGunConfig -> Pos2
shotBounceReflectOffSurfaceTargetPos shotHbx shotPrevBounceData cfg = shotEndPos `vecAdd` r'
    where
        shotStartPos                 = hitboxStartVertex shotHbx
        shotEndPos@(Pos2 shotEndX _) = hitboxEndVertex shotHbx

        isHorizReflect = \hbx -> shotEndX `approxEq` hitboxLeft hbx || shotEndX `approxEq` hitboxRight hbx
        Pos2 dx dy     = shotEndPos `vecSub` shotStartPos
        r              = case shotPrevBounceData of
            PrevBounceSurfaceHitbox surfaceHbx
                | isHorizReflect surfaceHbx -> Pos2 (-dx) dy
            _                               -> Pos2 dx (-dy)
        r'             = vecNormalize r `vecMul` _maxBounceRange cfg

shotBounceToSurfaceTargetPos :: Hitbox -> RicochetGunConfig -> Pos2
shotBounceToSurfaceTargetPos shotHbx cfg
    | shotEndX < shotStartX =
        let
            angleVec     = vecNormalize $ Pos2 (-1.0) 1.0
            bounceOffset = angleVec `vecMul` _shootRange cfg
        in shotEndPos `vecAdd` bounceOffset
    | otherwise             =
        let
            angleVec     = vecNormalize $ Pos2 1.0 1.0
            bounceOffset = angleVec `vecMul` _shootRange cfg
        in shotEndPos `vecAdd` bounceOffset
    where
        shotStartX                   = vecX $ hitboxStartVertex shotHbx
        shotEndPos@(Pos2 shotEndX _) = hitboxEndVertex shotHbx

calculatePlayerAimBody
    :: RicochetGunConfig
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
