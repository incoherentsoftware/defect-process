module Particle.All.EnemyHurt
    ( mkEnemyHurtParticle
    , mkEnemyHurtParticleEx
    ) where

import Control.Monad          (unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Random   (MonadRandom)
import Data.Foldable          (for_)
import Data.Maybe             (fromMaybe, listToMaybe)
import System.Random          (randomRIO)
import System.Random.Shuffle  (shuffleM)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V

import Attack
import Attack.Hit
import Collision.Hitbox
import Configs
import Configs.All.Particle
import Constants
import Enemy
import FileCache
import Particle as P
import Util
import Window.Graphics
import World.ZIndex

speckNormalHitAngles        = map (toRadians . Degrees) [0, 20..360] :: [Radians]
speckStrongHitAngles        = speckNormalHitAngles                   :: [Radians]
speckWeakHitAngles          = map (toRadians . Degrees) [0, 60..360] :: [Radians]
speckWeakHitScaleMultiplier = 0.4                                    :: Float

streakNormalHitCount         = 7    :: Int
streakStrongHitCount         = 15   :: Int
streakNormalHitAngleModifier = 0.25 :: Float
streakStrongHitAngleModifier = 0.05 :: Float

packPath                  = \f -> PackResourceFilePath "data/particles/particles-enemy.pack" f
enemyHurtWeakParticlePath = packPath "enemy-hurt-weak.spr" :: PackResourceFilePath

streakShortParticlePaths = map packPath
    [ "enemy-hurt-a.spr"
    , "enemy-hurt-b.spr"
    , "enemy-hurt-c.spr"
    , "enemy-hurt-d.spr"
    , "enemy-hurt-e.spr"
    , "enemy-hurt-f.spr"
    , "enemy-hurt-g.spr"
    , "enemy-hurt-h.spr"
    , "enemy-hurt-i.spr"
    ] :: [PackResourceFilePath]

speckSmallParticlePaths = NE.fromList $ map packPath
    [ "enemy-speck-small-a.spr"
    , "enemy-speck-small-b.spr"
    , "enemy-speck-small-c.spr"
    , "enemy-speck-small-d.spr"
    , "enemy-speck-small-e.spr"
    , "enemy-speck-small-f.spr"
    , "enemy-speck-small-g.spr"
    ] :: NE.NonEmpty PackResourceFilePath

data Streak = Streak
    { _angle  :: Radians
    , _sprite :: Sprite
    }

mkStreaks
    :: (FileCache m, GraphicsRead m, MonadIO m, MonadRandom m)
    => AttackHitEffectType
    -> m (V.Vector Streak)
mkStreaks hitEffectType = do
    streakSprs <- traverse loadPackSprite =<< case hitEffectType of
        NormalHitEffect -> take streakNormalHitCount . cycle <$> shuffleM streakShortParticlePaths
        StrongHitEffect -> take streakStrongHitCount . cycle <$> shuffleM streakShortParticlePaths
        WeakHitEffect   -> return [enemyHurtWeakParticlePath]

    let
        n                     = length streakSprs
        streakBaseAngle       = 2 * pi / fromIntegral n
        streakBaseAngleOffset = streakBaseAngle * case hitEffectType of
            NormalHitEffect -> streakNormalHitAngleModifier
            StrongHitEffect -> streakStrongHitAngleModifier
            WeakHitEffect   -> 0.0

    streakInitialAngle <- randomRIO (0.0, streakBaseAngle)
    streakAngles       <- sequence
        [ randomRIO (angle - streakBaseAngleOffset, angle + streakBaseAngleOffset)
        | i <- [0..n - 1]
        , let angle = streakInitialAngle + streakBaseAngle * fromIntegral i
        ]
    return $ V.fromList [Streak angle spr | (angle, spr) <- zip streakAngles streakSprs]

updateStreak :: Streak -> Streak
updateStreak streak = streak {_sprite = spr}
    where spr = updateSprite $ _sprite (streak :: Streak)

data Speck = Speck
    { _pos         :: Pos2
    , _gravityVelY :: VelY
    , _vec         :: Vec2
    , _speed       :: Speed
    , _angle       :: Radians
    , _sprite      :: Sprite
    }

mkSpeck :: (FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> Radians -> Speed -> Float -> ParticleConfig -> m Speck
mkSpeck pos angle initialSpeed scale cfg = do
    initialOffset <- randomRIO
        ( _enemyHurtSpeckInitialOffsetLow cfg * scale
        , _enemyHurtSpeckInitialOffsetHigh cfg * scale
        )
    let
        offset = Pos2 (initialOffset * cos angle) (initialOffset * sin angle)
        vec    = Vec2 (cos angle) (sin angle)

    spr <- loadPackSprite =<< randomChoice speckSmallParticlePaths

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
    -> Float
    -> AttackHitEffectType
    -> ParticleConfig
    -> m (V.Vector Speck)
mkSpecks pos scale hitEffectType cfg =
    let
        (speckAngles, scale') = case hitEffectType of
            NormalHitEffect -> (speckNormalHitAngles, scale)
            StrongHitEffect -> (speckStrongHitAngles, scale)
            WeakHitEffect   -> (speckWeakHitAngles, scale * speckWeakHitScaleMultiplier)
    in do
        speckInitialAngle <- randomRIO (0.0, fromMaybe 0.0 (listToMaybe speckAngles))
        angles            <- sequence
            [ randomRIO (speckInitialAngle + startAng, speckInitialAngle + endAng)
            | (startAng, endAng) <- zip speckAngles (tail speckAngles)
            ]

        initialSpeeds <- sequence
            [ randomRIO (_enemyHurtSpeckInitialSpeedLow cfg, _enemyHurtSpeckInitialSpeedHigh cfg * scale')
            | _ <- [0..length angles - 1]
            ]

        V.fromList <$> sequence
            [ mkSpeck pos angle initialSpeed scale' cfg
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
        gravityVelY = _gravityVelY speck + _enemyHurtSpeckGravity cfg * timeStep
        vel         = speckVelocity $ speck {_gravityVelY = gravityVelY}
        pos'        = pos `vecAdd` (toPos2 $ vel `vecMul` timeStep)
        acc         = _enemyHurtSpeckAcceleration cfg
        speed'      = max 0.0 (speed + acc * timeStep)

speckVelocity :: Speck -> Vel2
speckVelocity speck = vel `vecAdd` (Vel2 0.0 gravityVelY)
    where
        vel         = toVel2 $ _vec speck `vecMul` _speed speck
        gravityVelY = _gravityVelY speck

data HurtParticleData = HurtParticleData
    { _drawScale :: DrawScale
    , _streaks   :: V.Vector Streak
    , _specks    :: V.Vector Speck
    , _config    :: ParticleConfig
    }

mkEnemyHurtParticle
    :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m, MonadRandom m)
    => Enemy d
    -> AttackHit
    -> EnemyHurtEffectData
    -> m (Some Particle)
mkEnemyHurtParticle enemy atkHit hurtEffectData = mkEnemyHurtParticleEx enemy atkHit hurtEffectData hitEffectType
    where hitEffectType = _hitEffectType (atkHit :: AttackHit)

mkEnemyHurtParticleEx
    :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m, MonadRandom m)
    => Enemy d
    -> AttackHit
    -> EnemyHurtEffectData
    -> AttackHitEffectType
    -> m (Some Particle)
mkEnemyHurtParticleEx enemy _ hurtEffectData hitEffectType = do
    streaks <- mkStreaks hitEffectType

    let
        drawScale = case hitEffectType of
            NormalHitEffect -> _drawScale (hurtEffectData :: EnemyHurtEffectData)
            StrongHitEffect -> _strongDrawScale hurtEffectData
            WeakHitEffect   -> _drawScale (hurtEffectData :: EnemyHurtEffectData)  -- reuse normal hit scale for now

    let pos = hitboxCenter $ enemyHitbox enemy
    cfg    <- _particle <$> readConfigs
    specks <- mkSpecks pos (drawScaleToFloat drawScale) hitEffectType cfg

    let
        particleData = HurtParticleData
            { _drawScale = drawScale
            , _streaks   = streaks
            , _specks    = specks
            , _config    = cfg
            }

    return . Some $ (mkParticle particleData pos maxSecs)
        { _draw   = draw
        , _update = update
        }

draw :: (GraphicsReadWrite m, MonadIO m) => ParticleDraw HurtParticleData m
draw particle =
    let
        streakPos    = P._pos particle
        particleData = P._data particle
        streaks      = _streaks (particleData :: HurtParticleData)
        drawScale    = _drawScale (particleData :: HurtParticleData)
    in do
        for_ streaks $ \streak ->
            let
                angle = _angle (streak :: Streak)
                spr   = _sprite (streak :: Streak)
            in unless (spriteFinished spr) $
                drawSpriteEx streakPos RightDir enemyHurtParticleZIndex angle FullOpacity drawScale spr

        for_ (_specks particleData) $ \speck ->
            let
                pos   = _pos (speck :: Speck)
                vel   = speckVelocity speck
                angle = _angle (speck :: Speck)
                spr   = _sprite (speck :: Speck)
            in do
                speckPos' <- graphicsLerpPos pos vel
                drawSpriteRotated speckPos' RightDir enemyHurtParticleZIndex angle spr

update :: ParticleUpdate HurtParticleData
update particle = particle
    { _data = particleData'
    , _ttl  = ttl
    }
    where
        particleData = P._data particle
        streaks      = V.map updateStreak (_streaks particleData)
        specks       = V.filter (not . spriteFinished . (_sprite :: Speck -> Sprite)) (_specks particleData)
        cfg          = _config particleData
        specks'      = V.map (updateSpeck cfg) specks

        particleData' = particleData
            { _streaks = streaks
            , _specks  = specks'
            }

        streaksFinished = and $ V.map spriteFinished (V.map (_sprite :: Streak -> Sprite) streaks)
        ttl
            | streaksFinished && null specks' = 0.0
            | otherwise                       = maxSecs
