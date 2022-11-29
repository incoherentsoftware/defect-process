module Particle.All.AttackSpecks
    ( module Particle.All.AttackSpecks.Types
    , mkAttackSpecksParticle
    , mkAttackSpecksParticleEx
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (for_)
import Data.Maybe             (fromMaybe)
import System.Random          (randomRIO)
import qualified Data.Vector as V

import Attack
import Attack.Hit
import Collision.Hitbox
import Configs
import Configs.All.Particle
import Constants
import FileCache
import Particle as P
import Particle.All.AttackSpecks.Types
import Particle.All.AttackSpecks.Util
import Util
import Window.Graphics
import World.ZIndex

speckWeakHitAnyAngles     = map (toRadians . Degrees) [0, 120..360]           :: [Radians]
speckNormalHitAnyAngles   = map (toRadians . Degrees) [0, 60..360]            :: [Radians]
speckStrongHitAnyAngles   = map (toRadians . Degrees) [0, 20..360]            :: [Radians]
speckNormalHitLeftAngles  = map (toRadians . Degrees . (+ 90)) [40, 65..140]  :: [Radians]
speckStrongHitLeftAngles  = map (toRadians . Degrees . (+ 90)) [40, 50..140]  :: [Radians]
speckNormalHitRightAngles = map (subtract pi) speckNormalHitLeftAngles        :: [Radians]
speckStrongHitRightAngles = map (subtract pi) speckStrongHitLeftAngles        :: [Radians]
speckStrongHitUpAngles    = map (toRadians . Degrees . (+ 180)) [00, 20..180] :: [Radians]

calculateSpeckAngles :: AttackSpecksDirection -> AttackHitEffectType -> [Radians]
calculateSpeckAngles specksDir hitEffectType = case (specksDir, hitEffectType) of
    (SpecksAnyDir, StrongHitEffect)   -> speckStrongHitAnyAngles
    (SpecksAnyDir, NormalHitEffect)   -> speckNormalHitAnyAngles
    (SpecksAnyDir, WeakHitEffect)     -> speckWeakHitAnyAngles
    (SpecksLeftDir, StrongHitEffect)  -> speckStrongHitLeftAngles
    (SpecksLeftDir, _)                -> speckNormalHitLeftAngles
    (SpecksRightDir, StrongHitEffect) -> speckStrongHitRightAngles
    (SpecksRightDir, _)               -> speckNormalHitRightAngles
    (SpecksUpDir, _)                  -> speckStrongHitUpAngles

calculateSpeckInitialOffsets :: AttackSpecksType -> ParticleConfig -> (Distance, Distance)
calculateSpeckInitialOffsets specksType cfg = case specksType of
    SwordSpecksType        -> (_swordSpeckInitialOffsetLow cfg, _swordSpeckInitialOffsetHigh cfg)
    StaffSpecksType        -> (_staffSpeckInitialOffsetLow cfg, _staffSpeckInitialOffsetHigh cfg)
    GauntletsSpecksType    -> (_gauntletsSpeckInitialOffsetLow cfg, _gauntletsSpeckInitialOffsetHigh cfg)
    ScytheSpecksType       -> (_scytheSpeckInitialOffsetLow cfg, _scytheSpeckInitialOffsetHigh cfg)
    SpiritBladeSpecksType  -> (_spiritBladeSpeckInitialOffsetLow cfg, _spiritBladeSpeckInitialOffsetHigh cfg)
    BulletSpecksType       -> (_bulletSpeckInitialOffsetLow cfg, _bulletSpeckInitialOffsetHigh cfg)
    ShardSpecksType        -> (_shardSpeckInitialOffsetLow cfg, _shardSpeckInitialOffsetHigh cfg)
    ShardExplodeSpecksType -> (_shardExplodeSpeckInitialOffsetLow cfg, _shardExplodeSpeckInitialOffsetHigh cfg)
    SpikeSpecksType        -> (_spikeSpeckInitialOffsetLow cfg, _spikeSpeckInitialOffsetHigh cfg)
    RicochetSpecksType     -> (_ricochetSpeckInitialOffsetLow cfg, _ricochetSpeckInitialOffsetHigh cfg)
    GrenadeSpecksType      -> (_grenadeSpeckInitialOffsetLow cfg, _grenadeSpeckInitialOffsetHigh cfg)
    MineSpecksType         -> (_mineSpeckInitialOffsetLow cfg, _mineSpeckInitialOffsetHigh cfg)
    GoldSpecksType         -> (_goldSpeckInitialOffsetLow cfg, _goldSpeckInitialOffsetHigh cfg)
    GrappleSpecksType      -> (_grappleSpeckInitialOffsetLow cfg, _grappleSpeckInitialOffsetHigh cfg)

data Speck = Speck
    { _pos         :: Pos2
    , _gravityVelY :: VelY
    , _vec         :: Vec2
    , _speed       :: Speed
    , _angle       :: Radians
    , _sprite      :: Sprite
    }

mkSpeck
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => AttackSpecksType
    -> Pos2
    -> Radians
    -> Speed
    -> ParticleConfig
    -> m Speck
mkSpeck specksType pos angle initialSpeed cfg = do
    initialOffset <- randomRIO $ calculateSpeckInitialOffsets specksType cfg
    let
        offset = Pos2 (initialOffset * cos angle) (initialOffset * sin angle)
        vec    = Vec2 (cos angle) (sin angle)

    spr <- loadPackSprite =<< randomChoice (attackSpecksParticlePaths specksType)

    return $ Speck
        { _pos         = pos `vecAdd` offset
        , _gravityVelY = 0.0
        , _vec         = vec
        , _speed       = initialSpeed
        , _angle       = angle
        , _sprite      = spr
        }

mkSpecks
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => Pos2
    -> AttackHitEffectType
    -> AttackHit
    -> ParticleConfig
    -> m (V.Vector Speck)
mkSpecks pos hitEffectType attackHit cfg =
    let
        atkDir      = _dir (attackHit :: AttackHit)
        specksDir   = case _specksDirection (attackHit :: AttackHit) of
            Nothing    -> toAttackSpecksDirection atkDir
            Just spDir -> spDir
        speckAngles = calculateSpeckAngles specksDir hitEffectType
    in case _specksType (attackHit :: AttackHit) of
        Nothing         -> return V.empty
        Just specksType -> do
            angles <- sequence
                [ randomRIO (startAng, endAng)
                | (startAng, endAng) <- zip speckAngles (tail speckAngles)
                ]

            initialSpeeds <- sequence
                [ randomRIO (_attackSpeckInitialSpeedLow cfg, _attackSpeckInitialSpeedHigh cfg)
                | _ <- [0..length angles - 1]
                ]

            V.fromList <$> sequence
                [ mkSpeck specksType pos angle initialSpeed cfg
                | (angle, initialSpeed) <- zip angles initialSpeeds
                ]

updateSpeck :: ParticleConfig -> Speck -> Speck
updateSpeck cfg speck = speck
    { _pos         = pos'
    , _gravityVelY = gravityVelY
    , _speed       = speed'
    , _sprite      = updateSprite $ _sprite (speck :: Speck)
    }
    where
        pos         = _pos (speck :: Speck)
        speed       = _speed speck
        gravityVelY = _gravityVelY speck + _attackSpeckGravity cfg * timeStep
        vel         = speckVelocity $ speck {_gravityVelY = gravityVelY}
        pos'        = pos `vecAdd` (toPos2 $ vel `vecMul` timeStep)
        acc         = _attackSpeckAcceleration cfg
        speed'      = max 0.0 (speed + acc * timeStep)

speckVelocity :: Speck -> Vel2
speckVelocity speck = vel `vecAdd` (Vel2 0.0 gravityVelY)
    where
        vel         = toVel2 $ _vec speck `vecMul` _speed speck
        gravityVelY = _gravityVelY speck

data ParticleData = ParticleData
    { _specks :: V.Vector Speck
    , _config :: ParticleConfig
    }

mkAttackSpecksParticle :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => AttackHit -> m (Some Particle)
mkAttackSpecksParticle attackHit = mkAttackSpecksParticleEx attackHit hitEffectType
    where hitEffectType = _hitEffectType (attackHit :: AttackHit)

mkAttackSpecksParticleEx
    :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m)
    => AttackHit
    -> AttackHitEffectType
    -> m (Some Particle)
mkAttackSpecksParticleEx attackHit hitEffectType = do
    let
        pos = case fromMaybe SpecksAtkIntersectPos (_specksPos (attackHit :: AttackHit)) of
            SpecksAtkIntersectPos -> _intersectPos attackHit
            SpecksAtkCenterPos    -> maybe (_intersectPos attackHit) hitboxCenter (_hitbox attackHit)
    cfg    <- _particle <$> readConfigs
    specks <- mkSpecks pos hitEffectType attackHit cfg

    let
        particleData = ParticleData
            { _specks = specks
            , _config = cfg
            }

    return . Some $ (mkParticle particleData pos maxSecs)
        { _draw   = draw
        , _update = update
        }

draw :: (GraphicsReadWrite m, MonadIO m) => ParticleDraw ParticleData m
draw particle =
    let specks = _specks $ P._data particle
    in for_ specks $ \speck ->
        let
            pos   = _pos (speck :: Speck)
            vel   = speckVelocity speck
            angle = _angle (speck :: Speck)
            spr   = _sprite (speck :: Speck)
        in do
            speckPos' <- graphicsLerpPos pos vel
            drawSpriteRotated speckPos' RightDir playerAttackEffectZIndex angle spr

update :: ParticleUpdate ParticleData
update particle = particle
    { _data = particleData'
    , _ttl  = ttl
    }
    where
        particleData = P._data particle
        specks       = V.filter (not . spriteFinished . (_sprite :: Speck -> Sprite)) (_specks particleData)
        cfg          = _config particleData
        specks'      = V.map (updateSpeck cfg) specks

        particleData' = particleData
            { _specks = specks'
            }

        ttl
            | null specks' = 0.0
            | otherwise    = maxSecs
